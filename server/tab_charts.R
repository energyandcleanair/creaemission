


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

emissions <- reactive({
  # req(emissions_all())
  req(input$pollutant)
  req(input$country)
  req(emissions_year())
  # print(input$pollutant)
  # emissions_all() %>%
  #   filter(poll==input$pollutant) %>%
  #   filter(input$country=="World" | country==input$country)
  # emissions <- if(input$region_type == 'Countries'){
  emissions_year() %>%
    filter(poll==input$pollutant) %>%
    filter(input$country=="World" | country==input$country)
  # } else {
  #   emissions_cities() %>%
  #     filter(poll == input$pollutant) %>%
  #     filter(input$city == 'All cities' | city %in% input$city) %>%
  #     filter(measurement == input$measurement)
  # }
  # emissions %>% filter(measurement == input$measurement)
})


output$plot <- renderPlotly({

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type
  # e <- emissions() %>% filter(year==2019)

  req(emissions_year())
  req(group_by)
  req(color_by)
  req(chart_type)

  e <- emissions_year()

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
       geom_bar(aes(fill=reorder(color, value), text=paste(paste0(color, " (2019)"), sprintf("%.2f kt", value), sep="\n")), stat="identity") +
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

  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }

  return(ggplotly(plt, tooltip="text") %>% reverse_legend_labels())

})


# region_type <- reactive({
#   if(input$region_type == 'Countries'){
#     countries <- c("World", unique(emissions_all()$country))
#     countries <- countries[!is.na(countries)]
#     pollutants <- c("NOx"="nox","SO2"="so2","CH4"="ch4","CO2"="co2")
#     color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
#     group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
#     # updateSelectInput(inputId = 'select_country', choices = countries)
#     # updateSelectInput(inputId = 'select_city', choices = c(''))
#     list('countries' = countries, 'pollutants' = pollutants, 'color_bys' = color_bys, 'group_bys' = group_bys)
#   } else {
#     cities <- c('All cities',
#                 sort(unique(emissions_cities()$city)))
#     pollutants <- c("NOx"="nox","SO2"="so2")
#     color_bys <- c("City"="city", "Sector"="sector")
#     group_bys <- c("City"="city", "Sector"="sector")
#     # updateSelectInput(inputId = 'select_city', choices = cities)
#     # updateSelectInput(inputId = 'select_country', choices = c(''))
#     list('cities' = cities, 'pollutants' = pollutants, 'color_bys' = color_bys, 'group_bys' = group_bys)
#   }
#   # updateSelectInput(inputId = 'pollutant', choices = pollutants)
#   # updateSelectInput(inputId = 'color_by', choices = color_bys)
#   # updateSelectInput(inputId = 'group_by', choices = group_bys)
# })

# change other input fields based on region_type

# observeEvent(region_type(), {
#   choices <- region_type()
#   if(input$region_type == 'Countries'){
#     updateSelectInput(inputId = 'country', choices = choices$countries)
#     updateSelectInput(inputId = 'city', choices = c(''))
#   } else {
#     updateSelectInput(inputId = 'city', choices = choices$cities)
#     updateSelectInput(inputId = 'country', choices = c(''))
#   }
#   updateSelectInput(inputId = 'pollutant', choices = choices$pollutants)
#   updateSelectInput(inputId = 'color_by', choices = choices$color_bys, selected = choices$color_bys[2])
#   updateSelectInput(inputId = 'group_by', choices = choices$group_bys)
# })


output$selectYear <- renderUI({
  req(input$chart_type)
  # selectInput("year", "Year:", multiple=F, choices = rev(get_emissions_years()), selected=max(get_emissions_years())),
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
    # pollutants <- c("NOx"="nox","SO2"="so2","CH4"="ch4","CO2"="co2")
    # color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
    # group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
    # # updateSelectInput(inputId = 'select_country', choices = countries)
    # # updateSelectInput(inputId = 'select_city', choices = c(''))
    # list('countries' = countries, 'pollutants' = pollutants, 'color_bys' = color_bys, 'group_bys' = group_bys)
  # } else {
  #   pollutants <- c("NOx"="nox","SO2"="so2","CO2"="co2")
  #   color_bys <- c("City"="city", "Sector"="sector")
  #   group_bys <- c("City"="city", "Sector"="sector")
  #   updateSelectInput(inputId = 'pollutant', choices = pollutants)
  #   updateSelectInput(inputId = 'color_by', choices = color_bys, selected = 'sector')
  #   updateSelectInput(inputId = 'group_by', choices = group_bys)
  #
  #   cities <- c('All cities',
  #               sort(unique(emissions_cities()$city)))
  #   selectInput('city', 'City', choices = cities, selected = 'All cities', multiple = T)

    # updateSelectInput(inputId = 'select_city', choices = cities)
    # updateSelectInput(inputId = 'select_country', choices = c(''))
    # list('cities' = cities, 'pollutants' = pollutants, 'color_bys' = color_bys, 'group_bys' = group_bys)
  # }
  # updateSelectInput(inputId = 'pollutant', choices = pollutants)
  # updateSelectInput(inputId = 'color_by', choices = color_bys)
  # updateSelectInput(inputId = 'group_by', choices = group_bys)
})

