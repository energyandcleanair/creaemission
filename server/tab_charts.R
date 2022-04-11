


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
emissions_all <- reactive({
  get_emissions()
})


emissions_cities <- reactive({
  get_emissions_cities()
})


emissions <- reactive({
  req(emissions_all())
  req(input$pollutant)
  req(input$country)
  print(input$pollutant)
  # emissions_all() %>%
  #   filter(poll==input$pollutant) %>%
  #   filter(input$country=="World" | country==input$country)
  if(input$region_type == 'Countries'){
    emissions_all() %>%
      filter(poll==input$pollutant) %>%
      filter(input$country=="World" | country==input$country)
  } else {
    emissions_cities() %>%
      filter(poll == input$pollutant) %>%
      filter(input$city == 'All cities' | city == input$city)
  }
})


output$plot <- renderPlotly({

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type
  # e <- emissions() %>% filter(year==2019)
  e <- if(input$region_type == 'Countries'){
    emissions() %>% filter(year==2019)
  } else {
    emissions()
  }

  req(e)
  req(group_by)
  req(color_by)
  req(chart_type)

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


    plt <- e_plt %>%
      ggplot(aes(value, group)) +
       geom_bar(aes(fill=reorder(color, value), text=paste(paste0(color, " (2019)"), sprintf("%.2f kt", value), sep="\n")), stat="identity") +
       # geom_text(aes(label=ifelse(value_pct<0.01,"",scales::percent(value_pct, accuracy=.1))), vjust=1, nudge_y = -500, col="white", size=4) +
       rcrea::theme_crea() +
       scale_x_continuous(expand = expansion(mult=c(0, 0.1)),
                           labels = scales::comma_format(suffix=" kt")) +
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


region_type <- reactive({
  if(input$region_type == 'Countries'){
    countries <- c("World", unique(emissions_all()$country))
    countries <- countries[!is.na(countries)]
    pollutants <- c("NOx"="nox","SO2"="so2","CH4"="ch4","CO2"="co2")
    color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
    group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
    # updateSelectInput(inputId = 'select_country', choices = countries)
    # updateSelectInput(inputId = 'select_city', choices = c(''))
    list('countries' = countries, 'pollutants' = pollutants, 'color_bys' = color_bys, 'group_bys' = group_bys)
  } else {
    cities <- c('All cities',
                sort(unique(emissions_cities()$city)))
    pollutants <- c("NOx"="nox","SO2"="so2")
    color_bys <- c("City"="city", "Sector"="sector")
    group_bys <- c("City"="city", "Sector"="sector")
    # updateSelectInput(inputId = 'select_city', choices = cities)
    # updateSelectInput(inputId = 'select_country', choices = c(''))
    list('cities' = cities, 'pollutants' = pollutants, 'color_bys' = color_bys, 'group_bys' = group_bys)
  }
  # updateSelectInput(inputId = 'pollutant', choices = pollutants)
  # updateSelectInput(inputId = 'color_by', choices = color_bys)
  # updateSelectInput(inputId = 'group_by', choices = group_bys)
})

# change other input fields based on region_type
observeEvent(region_type(), {
  choices <- region_type()
  if(input$region_type == 'Countries'){
    updateSelectInput(inputId = 'country', choices = choices$countries)
    updateSelectInput(inputId = 'city', choices = c(''))
  } else {
    updateSelectInput(inputId = 'city', choices = choices$cities)
    updateSelectInput(inputId = 'country', choices = c(''))
  }
  updateSelectInput(inputId = 'pollutant', choices = choices$pollutants)
  updateSelectInput(inputId = 'color_by', choices = choices$color_bys, selected = choices$color_bys[2])
  updateSelectInput(inputId = 'group_by', choices = choices$group_bys)
})


