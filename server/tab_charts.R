


# Download Handlers ----------------------------------
# Downloadable csv of selected dataset
output$download_csv <- downloadHandler(
  filename = function() {
    paste("emissions.csv", sep = "")
  },
  content = function(file) {
    write.csv(emissions(), file, row.names = FALSE)
  }
)


emissions_year <- reactive({
  req(input$year)
  get_emissions_by_year(input$year)
})


emissions_years <- reactive({
  req(input$country)
  isos <- input$country
  if("all" %in% isos){
    # Return topn countries
    isos_top_n <- get_emissions_by_year(year=2022) %>%
      group_by(iso) %>%
      summarise(value=sum(value, na.rm=T)) %>%
      arrange(desc(value)) %>%
      head(topn) %>%
      pull(iso)

    # Remove all and replace with topn
    isos <- isos[isos!="all"]
    isos <- c(isos_top_n, isos)
    isos <- unique(isos)
  }
  get_emissions_by_countries(isos=isos)
})


emissions <- reactive({
  # req(emissions_all())
  req(input$pollutant)
  req(input$country)
  req(input$chart_type)

  single_year_charts <- c("barh")
  multiple_year_charts <- c("area")

  e <- if(input$chart_type %in% single_year_charts){
    emissions_year()
  } else {
    emissions_years()
  }

  # Add World
  e <- e %>%
    bind_rows(
      e %>%
        group_by(poll, sector, fuel, units, year) %>%
        summarise(value=sum(value, na.rm=T)) %>%
        mutate(country="World", iso="world")
    )


  e %>%
    filter(poll==input$pollutant) %>%
    filter((input$country=="all" & iso != "world")| iso==input$country) %>%
    mutate(sector=clean_sector_name(sector),
           fuel=clean_fuel_name(fuel),
           country=clean_country_name(country))

})


output$plot <- renderPlotly({

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type

  req(emissions())
  req(group_by)
  req(color_by)
  req(chart_type)

  e <- emissions()

  # Assert that all units start with kt
  if(!all(grepl("^kt", e$units))){
    stop("Not all units are in kt")
  }
  unit_suffix <- "kt"
  e <- e %>%
    group_by_at(c(group_by, color_by, "year")) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    ungroup()


  if(chart_type=="barh"){

    e$group <- e %>% pull(group_by)
    e$color <- e %>% pull(color_by)

    # Top N only
    topn_groups <- e %>%
      group_by(group) %>%
      summarise(value_pct=sum(value)/sum(e$value)) %>%
      arrange(desc(value_pct)) %>%
      head(topn) %>%
      pull(group)

    e_plt <- e[e$group%in%topn_groups,] %>%
      filter(value>0)

    e_plt$group <- factor(e_plt$group, levels=rev(topn_groups))

    colourCount = length(unique(e_plt[[color_by]]))
    # getPalette = colorRampPalette(rcrea::pal_crea)
    getPalette = colorRampPalette(brewer.pal(12, "Paired"))

    # if(input$region_type == 'C40 Cities'){
    #   if(input$measurement == 'Absolute'){
    #     unit_suffix <- ' kt'
    #   } else {
    #     unit_suffix <- ' kg'
    #   }
    # } else {
      # unit_suffix <- ' kt'
    # }


    plt <- e_plt %>%
      ggplot(aes(value, group)) +
       geom_bar(aes(fill=reorder(color, value), text=paste(paste0(color), sprintf("%.2f kt", value), sep="\n")), stat="identity") +
       # geom_text(aes(label=ifelse(value_pct<0.01,"",scales::percent(value_pct, accuracy=.1))), vjust=1, nudge_y = -500, col="white", size=4) +
       rcrea::theme_crea() +
       scale_x_continuous(expand = expansion(mult=c(0, 0.1)),
                           labels = scales::comma_format(suffix=unit_suffix)) +
      scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
       labs(caption="Source: CREA analysis based on CEDS.",
            y=NULL,
            x=NULL)
  }

  if(chart_type=="area"){


    e$group <- e %>% pull(group_by)
    e$color <- e %>% pull(color_by)

    # Top N only
    topn_groups <- e %>%
      group_by(group) %>%
      summarise(value_pct=sum(value)/sum(e$value)) %>%
      arrange(desc(value_pct)) %>%
      head(topn) %>%
      pull(group)

    e_plt <- e[e$group%in%topn_groups,]

    # Remove those who only have zeros or na
    e_plt <- e_plt %>%
      group_by(group, color) %>%
      filter(sum(value, na.rm=T)>0) %>%
      ungroup()

    e_plt$group <- factor(e_plt$group, levels=rev(topn_groups))

    colourCount = length(unique(e_plt[[color_by]]))
    getPalette = colorRampPalette(brewer.pal(12, "Paired"))


    plt <- e_plt %>%
      mutate(color = reorder(color, value)) %>%
      ungroup() %>%
      ggplot(aes(year, value)) +
      geom_area(aes(fill=color,
                    text=paste(paste0(color)))
                ) +
      rcrea::theme_crea() +
      scale_y_continuous(expand = expansion(mult=c(0, 0.1)),
                         labels = scales::comma_format(suffix=unit_suffix)) +
      scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
      labs(caption="Source: CREA analysis based on CEDS.",
           y=NULL,
           x=NULL) +
      facet_wrap(~group, scales="free_y")

  }

  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }

  return(ggplotly(plt, tooltip="text") %>% reverse_legend_labels())

})


output$selectYear <- renderUI({
  req(input$chart_type)
  years <- get_emissions_years()
  if(input$chart_type == 'barh'){
    return(selectInput("year", "Year:", multiple=F, choices = rev(years), selected=max(years)))
  }
  if(input$chart_type == 'area'){
    # No select shown
    return(NULL)
  }
  return(NULL)
})

output$selectCountry <- renderUI({
  req(emissions_year())

  countries <- emissions_year() %>%
    # We'll want World at the top with All
    filter(iso!="world", !is.na(country)) %>%
    arrange(country) %>%
    distinct(country, iso) %>%
    # Transformed to named vector iso=country
    tibble::deframe()

  countries <- c("All"="all", "World"="world", countries)
  # countries <- countries[!is.na(countries)]
  selectInput('country', 'Country', choices = countries, multiple=T, selected='all')
})



clean_fuel_name <- function(x){
  gsub("_", "", tolower(x)) %>%
  stringr::str_to_sentence()
}

clean_sector_name <- function(x){
  # Extract first thing before _
  # x="1A1_Ind-comb"
  sector_id <- str_extract(x, "^[^_]+")

  x %>%
    # str_replace_all("Ind", "industry") %>%
    # str_replace_all("comb", "combustion") %>%
    # str_replace_all("prod", "production") %>%
    # str_replace_all("prodprod", "prod") %>%
    # remove sector_id
    str_remove(sector_id) %>%
    gsub("-|_", " ", .) %>%
    stringr::str_to_sentence() %>%
    gsub("^ ", "", .) %>%
    # add [sector_id] at the end
    paste0(" [", sector_id, "]")
}


clean_country_name <- function(x){
  # replace NA with International
  x[is.na(x)] <- "International"
  x
}
