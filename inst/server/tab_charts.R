# Add this near the top of the file, with other reactive declarations
selected_countries <- reactiveVal(NULL)
selected_year <- reactiveVal(NULL)
selected_pollutant <- reactiveVal("NOx")  # Default to NOx


# Add this observer to update the stored selection when input changes
observeEvent(input$country, {
  selected_countries(input$country)
})

observeEvent(input$year, {
  selected_year(input$year)
})

observeEvent(input$pollutant, {
  selected_pollutant(input$pollutant)
})

# Reactive for current source object
current_source <- reactive({
  req(input$source)
  req(input$region_type)

  source_obj <- get_current_source(input$source, input$region_type)

  return(source_obj)
})

# Function to validate and update selections when source changes
validate_and_update_selections <- function(new_source, new_region_type) {
  # Get current source object
  source_obj <- current_source()

  # Get available data from new source
  available_data <- source_obj$list_available_data()

  available_years <- sort(unique(available_data$year))
  available_pollutants <- unique(available_data$pollutant)
  available_countries <- available_data %>%
    distinct(iso3) %>%
    filter(iso3 != "world") %>%
    pull(iso3)

  # Validate year selection
  current_year <- selected_year()
  if (!is.null(current_year) && current_year %in% available_years) {
    # Keep current year if it's available
    year_to_use <- current_year
  } else {
    # Use latest available year as default
    year_to_use <- max(available_years)
    selected_year(year_to_use)
  }

  # Validate pollutant selection
  current_pollutant <- selected_pollutant()
  if (!is.null(current_pollutant) && current_pollutant %in% available_pollutants) {
    # Keep current pollutant if it's available
    pollutant_to_use <- current_pollutant
  } else {
    # Use NOx as default if available, otherwise first available pollutant
    if ("NOx" %in% available_pollutants) {
      pollutant_to_use <- "NOx"
    } else {
      pollutant_to_use <- available_pollutants[1]
    }
    selected_pollutant(pollutant_to_use)
  }

  # Validate country selection
  current_countries <- selected_countries()
  if (!is.null(current_countries)) {
    # Keep only countries that are available in the new source
    valid_countries <- current_countries[current_countries %in% available_countries]
    if (length(valid_countries) > 0) {
      # Keep valid countries
      countries_to_use <- valid_countries
    } else {
      # Use "all" as default for national, first country for provincial
      if (new_region_type == REGIONTYPE_NATIONAL) {
        countries_to_use <- "all"
      } else {
        countries_to_use <- available_countries[1]
      }
    }
  } else {
    # No previous selection, use defaults
    if (new_region_type == REGIONTYPE_NATIONAL) {
      countries_to_use <- "all"
    } else {
      countries_to_use <- available_countries[1]
    }
  }

  # Update stored selections
  selected_countries(countries_to_use)


  return(list(
    year = year_to_use,
    pollutant = pollutant_to_use,
    countries = countries_to_use
  ))
}

# Observer for source changes
observeEvent(input$source, {
  req(input$source)
  req(input$region_type)

  # Validate and update selections
  validated_selections <- validate_and_update_selections(input$source, input$region_type)

  # Update UI inputs with validated selections
  updateSelectInput(session, "year", selected = validated_selections$year)
  updateSelectInput(session, "pollutant", selected = validated_selections$pollutant)
  updateSelectInput(session, "country", selected = validated_selections$countries)
})

# Observer for region type changes
observeEvent(input$region_type, {
  req(input$source)
  req(input$region_type)

  # Validate and update selections
  validated_selections <- validate_and_update_selections(input$source, input$region_type)

  # Update UI inputs with validated selections
  updateSelectInput(session, "year", selected = validated_selections$year)
  updateSelectInput(session, "pollutant", selected = validated_selections$pollutant)
  updateSelectInput(session, "country", selected = validated_selections$countries)
})

# Download Handlers ----------------------------------
# Downloadable csv of selected dataset
output$download_csv <- downloadHandler(
  filename = function() {
    paste("emissions.csv", sep = "")
  },
  content = function(file) {
    data_to_write <- emissions()
    write.csv(data_to_write, file, row.names = FALSE)
  }
)


emissions_raw <- reactive({

  req(input$region_type)
  req(input$country)
  req(input$source)
  req(input$pollutant)
  topn <- input$topn

  start_time <- Sys.time()

  # Get current source object
  source_obj <- current_source()

  # Years
  single_year_charts <- c("barh")
  multiple_year_charts <- c("area")
  if(input$chart_type %in% single_year_charts){
    years <- input$year
    if(is.null(years)){
      # Probably being set up
      return(NULL)
    }
  } else {
    years <- NULL
  }

  # Adjust isos
  iso3s <- input$country
  if(!is.null(iso3s) & ("all" %in% iso3s)){
    # Get latest year from available data
    available_data <- source_obj$list_available_data()

    latest_year <- max(available_data$year)

    # Get top N countries for the latest year
    latest_data <- source_obj$get(year = latest_year, pollutant = input$pollutant)

    if (!is.null(latest_data) && nrow(latest_data) > 0) {
      iso3s_top_n <- latest_data %>%
        group_by(iso3) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        arrange(desc(value)) %>%
        head(topn) %>%
        pull(iso3)

      # Remove all and replace with topn
      iso3s <- iso3s[iso3s != "all"]
      iso3s <- c(iso3s_top_n, iso3s)
      iso3s <- unique(iso3s)
    }
  }

  # Get emissions data from source
  emissions_data <- source_obj$get(
    year = years,
    iso3 = iso3s,
    pollutant = input$pollutant,
  )

  # Return NULL if no data available
  if (is.null(emissions_data) || nrow(emissions_data) == 0) {
    return(NULL)
  }

  return(emissions_data)
})


emissions <- reactive({
  req(input$country)
  req(input$chart_type)
  req(emissions_raw())

  e <- emissions_raw()

  # Add region_name if not present
  if(!"region_name" %in% names(e)){
    e <- e %>%
      mutate(region_name = iso3_to_country(iso3))
  }

  # Aggregate
  group_cols <- c(input$group_by, input$color_by, "year", "region_name")
  e <- e %>%
    filter(("all" %in% input$country & iso3 != "world") | iso3 %in% input$country) %>%
    group_by_at(group_cols) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()


  return(e)
})


output$selectTopN <- renderUI({
  # Integer output
  numericInput("topn", "Top N:", value=20, min=1, max=100)
})

output$plot <- renderPlotly({

  start_time <- Sys.time()

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type
  topn <- input$topn

  req(emissions())
  req(group_by)
  req(color_by)
  req(chart_type)

  e <- emissions()

  # Check if we have data
  if (is.null(e) || nrow(e) == 0) {
    # Create an empty plot with a message
    empty_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected parameters",
               size = 6, color = "gray50") +
      theme_void() +
      theme(plot.background = element_rect(fill = "white"))

    return(ggplotly(empty_plot))
  }

  # Assert that all units start with kt or Gg
  if(!all(grepl("^kt|Gg", e$units))){
    stop("Not all units are in kt")
  }
  unit_suffix <- "kt"

  # Remove the second aggregation since we're already handling it in emissions() reactive
  # e <- e %>%
  #   group_by_at(c(group_by, color_by, "year")) %>%
  #   summarise(value=sum(value, na.rm=T)) %>%
  #   ungroup()


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

    # Clean and truncate long names for better tooltip display
    e_plt <- e_plt %>%
      mutate(
        # Clean the color names using utility functions
        color_clean = case_when(
          color_by == "sector" ~ clean_sector_name(color),
          color_by == "fuel" ~ clean_fuel_name(color),
          TRUE ~ color
        ),
        # Truncate very long names to prevent popup overflow
        color_display = ifelse(nchar(color_clean) > 40,
                              paste0(substr(color_clean, 1, 25), "..."),
                              color_clean),
        # Cheat: add trailing spaces to legend labels to avoid cropping in ggplotly
        color_label = paste0(color_clean, "  ")
      )

    plt <- e_plt %>%
      ggplot(aes(value, group)) +
       geom_bar(aes(fill=reorder(color_label, value), text=paste(paste0(color_display), sprintf("%.2f kt", value), sep="\n")), stat="identity") +
       rcrea::theme_crea_new() +
       scale_x_continuous(expand = expansion(mult=c(0, 0.1)),
                           labels = scales::comma_format(suffix=unit_suffix)) +
      scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
       labs(caption=paste0("Source: CREA analysis based on ", input$source, "."),
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

    # Clean and truncate long names for better tooltip display
    e_plt <- e_plt %>%
      mutate(
        # Clean the color names using utility functions
        color_clean = case_when(
          color_by == "sector" ~ clean_sector_name(color),
          color_by == "fuel" ~ clean_fuel_name(color),
          TRUE ~ color
        ),
        # Truncate very long names to prevent popup overflow
        color_display = ifelse(nchar(color_clean) > 40,
                              paste0(substr(color_clean, 1, 25), "..."),
                              color_clean),
        # Cheat: add trailing spaces to legend labels to avoid cropping in ggplotly
        color_label = paste0(color_clean, "  ")
      )


    plt <- e_plt %>%
      mutate(color_label = reorder(color_label, value)) %>%
      ungroup() %>%
      ggplot(aes(year, value)) +
      geom_area(aes(fill=color_label)) +
      rcrea::theme_crea_new() +
      scale_y_continuous(expand = expansion(mult=c(0, 0.1)),
                         labels = scales::comma_format(suffix=unit_suffix)) +
      scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
      labs(caption=paste0("Source: CREA analysis based on ", input$source, "."),
           y=NULL,
           x=NULL) +
      facet_wrap(~group, scales="free")

  }


  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }

  plotly_plot <- if(chart_type == "barh") {
    ggplotly(plt, tooltip="text") %>%
      reverse_legend_labels()
  } else if(chart_type == "area") {

    ggplotly(plt) %>%
      reverse_legend_labels() %>%
      fix_ggplotly_facets(
        hgap = 0.05,
        outer = c(0.01, 0.01),
        vgap = 0.08,
        vouter = c(0.02, 0.02),
        recenter_strips = TRUE,
        adjust_strip_rects = TRUE,
        y_title_standoff = 15,
        left_margin = 80,
        right_margin = 180,
        strip_position = "top",
        strip_row_offset = 0.02
      )
  }


  return(plotly_plot)

})


output$selectYear <- renderUI({
  req(input$chart_type)
  req(input$region_type)
  req(input$source)

  # Get current source object
  source_obj <- current_source()

  # Get available years from actual data
  available_data <- source_obj$list_available_data()

  years <- sort(unique(available_data$year))

  if(input$chart_type == 'barh'){
    latest_year <- max(years)

    # Get previously selected year if it's still available
    prev_selected <- selected_year()
    if(!is.null(prev_selected) && prev_selected %in% years) {
      selected <- prev_selected
    } else {
      selected <- latest_year
    }

    return(selectInput("year", "Year:",
                      multiple=F,
                      choices = rev(years),
                      selected=selected))
  }
  if(input$chart_type == 'area'){
    # No select shown
    return(NULL)
  }
  return(NULL)
})

output$selectCountry <- renderUI({
  req(input$region_type)
  req(input$source)

  # Get current source object
  source_obj <- current_source()

  multiple = (input$region_type==REGIONTYPE_NATIONAL)

  if(input$region_type==REGIONTYPE_NATIONAL){
    # Get countries from source
    available_data <- source_obj$list_available_data()

    countries <- available_data %>%
      distinct(iso3) %>%
      mutate(country = iso3_to_country(iso3)) %>%
      filter(iso3!="world", !is.na(country)) %>%
      arrange(country) %>%
      distinct(country, iso3) %>%
      # Transformed to named vector iso=country
      tibble::deframe()
    countries <- c("All"="all", "World"="world", countries)

    # Get previously selected countries that are still available
    prev_selected <- selected_countries()
    if(!is.null(prev_selected)) {
      # Keep only countries that are still available in the new source
      valid_selected <- prev_selected[prev_selected %in% countries]
      if(length(valid_selected) > 0) {
        selected <- valid_selected
      } else {
        selected <- 'all'
      }
    } else {
      selected <- 'all'
    }

    selectInput('country', 'Country', choices = countries, multiple=multiple, selected=selected)
  } else {
    # Get provincial countries from source
    available_data <- source_obj$list_available_data()

    countries <- available_data %>%
      distinct(iso3) %>%
      mutate(country = iso3_to_country(iso3)) %>%
      distinct(country, iso3) %>%
      tibble::deframe()

    # Get previously selected country if still available
    prev_selected <- selected_countries()
    if(!is.null(prev_selected)) {
      # Keep only countries that are still available in the new source
      valid_selected <- prev_selected[prev_selected %in% countries]
      if(length(valid_selected) > 0) {
        selected <- valid_selected
      } else {
        selected <- countries[[1]]
      }
    } else {
      selected <- countries[[1]]
    }

    selectInput('country', 'Country', choices = countries, multiple=multiple, selected=selected)
  }
})


output$selectPollutant <- renderUI({
  req(current_source())

  # Get current source object
  source_obj <- current_source()

  # Get available pollutants from source
  available_data <- source_obj$list_available_data()

  available_pollutants <- unique(available_data$pollutant)

  # Create choices directly from available pollutants (no global filtering)
  available_pollutants_choices <- available_pollutants
  names(available_pollutants_choices) <- available_pollutants

  # Get previously selected pollutant if it's still available
  prev_selected <- selected_pollutant()
  if(!is.null(prev_selected) && prev_selected %in% available_pollutants_choices) {
    selected <- prev_selected
  } else {
    # Use NOx as default if available, otherwise first available pollutant
    if ("NOx" %in% available_pollutants_choices) {
      selected <- "NOx"
    } else {
      selected <- available_pollutants_choices[1]
    }
  }

  # Debug output
  message(glue::glue("Charts pollutant UI: prev={prev_selected}, available={paste(available_pollutants_choices, collapse=', ')}, selected={selected}"))

  selectInput("pollutant", "Species:", multiple=F, choices = available_pollutants_choices, selected=selected)
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
