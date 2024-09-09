


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

# output$selectCountry <- renderUI({
#   req(emissions_all())
#
#   countries <- c("World", unique(emissions_all()$country))
#   countries <- countries[!is.na(countries)]
#
#   selectInput("country", "Country",
#               multiple=F,
#               choices=countries,
#               selected="world")
#
#
# })
#
# output$selectCity <- renderUI({
#   req(emissions_cities)
#
#   cities <- c('None', 'All cities',
#               sort(unique(emissions_cities()$city)))
#
#   selectInput('city', 'City',
#               multiple = F,
#               choices = cities,
#               selected = 'None')
# })

# Output Elements --------------------------------------


# emissions_all <- reactive({
#   get_emissions()
# })
#
#
# emissions_cities <- reactive({
#   get_emissions_cities()
# })


emissions_year <- reactive({
  req(input$year)
  get_emissions(input$year)
})


emissions_years <- reactive({
  lapply(seq(2000, 2022), get_emissions) %>%
    bind_rows()
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

  e %>%
    filter(poll==input$pollutant) %>%
    filter(input$country=="World" | country==input$country) %>%
    mutate(sector=clean_sector_name(sector),
           fuel=clean_fuel_name(fuel))

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
    summarise(value=sum(value, na.rm=T))


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

    e_plt <- e[e$group%in%topn_groups,] %>%
      filter(value>0)

    e_plt$group <- factor(e_plt$group, levels=rev(topn_groups))

    colourCount = length(unique(e_plt[[color_by]]))
    getPalette = colorRampPalette(brewer.pal(12, "Paired"))


    plt <- e_plt %>%
      ggplot(aes(year, value)) +
      geom_area(aes(fill=reorder(color, value),
                    text=paste(paste0(color), sprintf("%.2f kt", value), sep="\n"))) +
      # geom_text(aes(label=ifelse(value_pct<0.01,"",scales::percent(value_pct, accuracy=.1))), vjust=1, nudge_y = -500, col="white", size=4) +
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
  # if(input$region_type == 'Countries'){
    req(emissions_year())
    pollutants <- c("NOx"="NOx",
                    "SO2"="SO2",
                    "CH4"="CH4",
                    "CO2"="CO2",
                    "NH3"="NH3",
                    "NMVOC"="NMVOC",
                    "BC"="BC",
                    "CO"="CO",
                    "N2O"="N2O"
                    )
    color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
    group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
    updateSelectInput(inputId = 'pollutant', choices = pollutants)
    updateSelectInput(inputId = 'color_by', choices = color_bys, selected = 'sector')
    updateSelectInput(inputId = 'group_by', choices = group_bys)

    countries <- c("World", unique(emissions_year()$country))
    countries <- countries[!is.na(countries)]
    selectInput('country', 'Country', choices = countries)
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
