


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


emissions_raw <- reactive({

  req(input$region_type)
  req(input$country)
  topn <- input$topn
  # req(input$year)

  # Adjust isos
  iso3s <- input$country
  if(!is.null(iso3s) & ("all" %in% iso3s)){
    # Return topn countries
    iso3s_top_n <- get_emissions_national_by_year(year=2022) %>%
      group_by(iso) %>%
      summarise(value=sum(value, na.rm=T)) %>%
      arrange(desc(value)) %>%
      head(topn) %>%
      pull(iso)

    # Remove all and replace with topn
    iso3s <- iso3s[iso3s!="all"]
    iso3s <- c(iso3s_top_n, iso3s)
    iso3s <- unique(iso3s)
  }

  # Adjust years
  single_year_charts <- c("barh")
  multiple_year_charts <- c("area")
  if(input$chart_type %in% single_year_charts){
    years <- input$year
  } else {
    years <- NULL
  }

  get_emissions(region_type=input$region_type, iso3s=iso3s, years=years)

})


emissions <- reactive({

  req(input$pollutant)
  req(input$country)
  req(input$chart_type)
  req(emissions_raw())

  e <- emissions_raw()

  # Add World
  if(input$region_type=="country"){
    e <- e %>%
      bind_rows(
        e %>%
          group_by(poll, sector, fuel, units, year) %>%
          summarise(value=sum(value, na.rm=T)) %>%
          mutate(country="World", iso="world")
      )
  }

  e %>%
    filter(poll==input$pollutant) %>%
    filter(("all" %in% input$country & iso != "world") | iso %in% input$country) %>%
    mutate(sector=clean_sector_name(sector),
           fuel=clean_fuel_name(fuel),
           country=clean_country_name(country))

})


output$selectTopN <- renderUI({
  # Integer output
  numericInput("topn", "Top N:", value=20, min=1, max=100)
})

output$plot <- renderPlotly({

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type
  topn <- input$topn

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

    e_plt$group <- factor(e_plt$group, levels=topn_groups)

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
      facet_wrap(~group, scales="free")

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
  req(input$region_type)

  if(input$region_type=="country"){
    years <- get_national_emissions_years()
  }else{
    years <- get_provoncial_emissions_years()
  }

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
  req(input$region_type)

  multiple = (input$region_type=="country")

  if(input$region_type=="country"){
    countries <- get_countries() %>%
      # We'll want World at the top with All
      filter(iso!="world", !is.na(country)) %>%
      arrange(country) %>%
      distinct(country, iso) %>%
      # Transformed to named vector iso=country
      tibble::deframe()
    countries <- c("All"="all", "World"="world", countries)
    selectInput('country', 'Country', choices = countries, multiple=multiple, selected='all')
  } else {
    countries <- tibble(iso=get_countries_with_provincial_data()) %>%
      mutate(country=countrycode(iso, "iso3c", "country.name")) %>%
      distinct(country, iso) %>%
      tibble::deframe()
    selectInput('country', 'Country', choices = countries, multiple=multiple, selected=countries[[1]])
  }

})



clean_fuel_name <- function(x){
  gsub("_", " ", tolower(x)) %>%
  stringr::str_to_sentence()
}

clean_sector_name <- function(x){
  # Extract first thing before _
  # x="1A1_Ind-comb"
  sector_id <- str_extract(x, "^[^_]+")
  if (all(sector_id==x)){
    sector_id <- ""
  }

  x %>%
    str_replace_all("Industrial", "Industry") %>%
    gsub(sector_id, "", .) %>%
    gsub("-|_", " ", .) %>%
    gsub(" Sector","", .) %>%
    stringr::str_to_sentence() %>%
    gsub("^ ", "", .) %>%
    # add [sector_id] at the end
    {
      if(all(sector_id=="")){
        .
      }else{
        paste0(., " [", sector_id, "]")
      }
    }
}


clean_country_name <- function(x){
  # replace NA with International
  x[is.na(x)] <- "International"
  x
}

output$selectPollutant <- renderUI({
  selectInput("pollutant", "Species:", multiple=F, choices = pollutants, selected=pollutants[1])
})

output$selectColorBy <- renderUI({
  req(input$region_type)
  choices <- color_bys

  if(input$region_type=="province"){
     names(choices) <- gsub("Country", "Province", names(choices))
  }

  selectInput("color_by", "Color by:", multiple=F, choices = choices, selected=color_bys[2])
})

output$selectGroupBy <- renderUI({
  req(input$region_type)
  choices <- group_bys

  if(input$region_type=="province"){
    names(choices) <- gsub("Country", "Province", names(group_bys))
  }

  selectInput("group_by", "Group by:", multiple=F, choices = choices, selected=group_bys[1])
})
