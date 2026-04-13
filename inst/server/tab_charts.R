# Add this near the top of the file, with other reactive declarations
selected_countries <- reactiveVal(NULL)
selected_year <- reactiveVal(NULL)
selected_pollutant <- reactiveVal("NOx")  # Default to NOx

# Bindings to satisfy linters for dplyr NSE columns
iso3 <- NULL
value <- NULL


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
  message("📊 CHARTS: Starting list_available_data()...")
  list_start <- Sys.time()
  available_data <- source_obj$list_available_data()
  list_end <- Sys.time()
  list_duration <- round(as.numeric(difftime(list_end, list_start, units = "secs")), 3)
  message(sprintf("📊 CHARTS: list_available_data() took %ss", list_duration))

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

  # Determine internal country selection defaults (no UI)
  current_countries <- selected_countries()
  if (!is.null(current_countries)) {
    # Keep only countries that are available in the new source
    valid_countries <- current_countries[current_countries %in% available_countries]
    if (length(valid_countries) > 0) {
      countries_to_use <- valid_countries
    } else {
      if (new_region_type == creaemission::REGIONTYPE_NATIONAL) {
        countries_to_use <- "all"
      } else {
        countries_to_use <- available_countries[1]
      }
    }
  } else {
    if (new_region_type == creaemission::REGIONTYPE_NATIONAL) {
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
  iso3s <- selected_countries()
  if (is.null(iso3s)) {
    # Default when not yet set
    available_data <- source_obj$list_available_data()
    available_countries <- available_data %>%
      distinct(iso3) %>%
      filter(iso3 != "world") %>%
      pull(iso3)
    if (input$region_type == creaemission::REGIONTYPE_NATIONAL) {
      iso3s <- "all"
    } else {
      iso3s <- available_countries[1]
    }
    selected_countries(iso3s)
  }

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
  message(sprintf("📊 CHARTS: Loading data for %s, years: %s, countries: %s", 
                 input$pollutant, 
                 if(is.null(years)) "all" else paste(years, collapse=","),
                 paste(iso3s, collapse=",")))
  data_start <- Sys.time()
  emissions_data <- source_obj$get(
    year = years,
    iso3 = iso3s,
    pollutant = input$pollutant,
  )
  data_end <- Sys.time()
  data_duration <- round(as.numeric(difftime(data_end, data_start, units = "secs")), 3)
  message(sprintf("📊 CHARTS: Data loading from source took %ss", data_duration))

  # Return NULL if no data available
  if (is.null(emissions_data) || nrow(emissions_data) == 0) {
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
    message(sprintf("📊 CHARTS: Data loading failed after %ss", duration))
    return(NULL)
  }

  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  message(sprintf("📊 CHARTS: Data loading completed in %ss (%d rows)", duration, nrow(emissions_data)))
  return(emissions_data)
})


emissions <- reactive({
  req(input$chart_type)
  req(emissions_raw())

  start_time <- Sys.time()
  e <- emissions_raw()

  # Add region_name if not present
  region_name_start <- Sys.time()
  if(!"region_name" %in% names(e)){
    e <- e %>%
      mutate(region_name = creaemission::iso3_to_country(iso3))
  }
  region_name_end <- Sys.time()
  region_name_duration <- round(as.numeric(difftime(region_name_end, region_name_start, units = "secs")), 3)

  # Aggregate
  aggregation_start <- Sys.time()
  group_cols <- c(input$group_by, input$color_by, "year", "region_name")
  e <- e %>%
    filter(("all" %in% selected_countries() & iso3 != "world") | iso3 %in% selected_countries()) %>%
    group_by_at(group_cols) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()
  aggregation_end <- Sys.time()
  aggregation_duration <- round(as.numeric(difftime(aggregation_end, aggregation_start, units = "secs")), 3)

  end_time <- Sys.time()
  total_duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  message(sprintf("📊 CHARTS: emissions() - region_name: %ss, aggregation: %ss, total: %ss", region_name_duration, aggregation_duration, total_duration))

  return(e)
})


output$selectTopN <- renderUI({
  # Integer output
  numericInput("topn", "Top N:", value=20, min=1, max=100)
})

output$plot <- plotly::renderPlotly({

  start_time <- Sys.time()
  message("📊 CHARTS: Starting plot rendering...")

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type
  topn <- input$topn

  req(emissions())
  req(group_by)
  req(color_by)
  req(chart_type)

  # Data preparation timing
  data_prep_start <- Sys.time()
  e <- emissions()
  data_prep_end <- Sys.time()
  data_prep_duration <- round(as.numeric(difftime(data_prep_end, data_prep_start, units = "secs")), 3)
  message(sprintf("📊 CHARTS: Data preparation took %ss", data_prep_duration))

  # Check if we have data
  if (is.null(e) || nrow(e) == 0) {
    # Create an empty plot with a message
    empty_plot_start <- Sys.time()
    empty_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected parameters",
               size = 6, color = "gray50") +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    empty_plot_end <- Sys.time()
    empty_plot_duration <- round(as.numeric(difftime(empty_plot_end, empty_plot_start, units = "secs")), 3)
    
    plotly_start <- Sys.time()
    result <- plotly::ggplotly(empty_plot)
    plotly_end <- Sys.time()
    plotly_duration <- round(as.numeric(difftime(plotly_end, plotly_start, units = "secs")), 3)
    
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
    message(sprintf("📊 CHARTS: Empty plot - ggplot: %ss, plotly: %ss, total: %ss", empty_plot_duration, plotly_duration, duration))
    return(result)
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
    message("📊 CHARTS: Processing barh chart...")
    
    # Data processing timing
    data_proc_start <- Sys.time()
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
    getPalette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))

    # Clean and truncate long names for better tooltip display
    e_plt <- e_plt %>%
      mutate(
        # Clean the color names using utility functions
        color_clean = case_when(
          color_by == "sector" ~ creaemission::clean_sector_name(color),
          color_by == "fuel" ~ creaemission::clean_fuel_name(color),
          TRUE ~ color
        ),
        # Truncate very long names to prevent popup overflow
        color_display = ifelse(nchar(color_clean) > 40,
                              paste0(substr(color_clean, 1, 25), "..."),
                              color_clean),
        # Cheat: add trailing spaces to legend labels to avoid cropping in ggplotly
        color_label = paste0(color_clean, "  ")
      )
    data_proc_end <- Sys.time()
    data_proc_duration <- round(as.numeric(difftime(data_proc_end, data_proc_start, units = "secs")), 3)
    message(sprintf("📊 CHARTS: Barh data processing took %ss", data_proc_duration))

    # ggplot creation timing
    ggplot_start <- Sys.time()
    plt <- e_plt %>%
      ggplot2::ggplot(ggplot2::aes(value, group)) +
       ggplot2::geom_bar(ggplot2::aes(fill=reorder(color_label, value), text=paste(paste0(color_display), sprintf("%.2f kt", value), sep="\n")), stat="identity") +
       rcrea::theme_crea_new() +
       ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult=c(0, 0.1)),
                           labels = scales::comma_format(suffix=unit_suffix)) +
      ggplot2::scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
       ggplot2::labs(caption=paste0("Source: CREA analysis based on ", input$source, "."),
            y=NULL,
            x=NULL)
    ggplot_end <- Sys.time()
    ggplot_duration <- round(as.numeric(difftime(ggplot_end, ggplot_start, units = "secs")), 3)
    message(sprintf("📊 CHARTS: Barh ggplot creation took %ss", ggplot_duration))
  }

  if(chart_type=="area"){
    message("📊 CHARTS: Processing area chart...")
    
    # Data processing timing
    data_proc_start <- Sys.time()
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
    getPalette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))

    # Clean and truncate long names for better tooltip display
    e_plt <- e_plt %>%
      mutate(
        # Clean the color names using utility functions
        color_clean = case_when(
          color_by == "sector" ~ creaemission::clean_sector_name(color),
          color_by == "fuel" ~ creaemission::clean_fuel_name(color),
          TRUE ~ color
        ),
        # Truncate very long names to prevent popup overflow
        color_display = ifelse(nchar(color_clean) > 40,
                              paste0(substr(color_clean, 1, 25), "..."),
                              color_clean),
        # Cheat: add trailing spaces to legend labels to avoid cropping in ggplotly
        color_label = paste0(color_clean, "  ")
      )
    data_proc_end <- Sys.time()
    data_proc_duration <- round(as.numeric(difftime(data_proc_end, data_proc_start, units = "secs")), 3)
    message(sprintf("📊 CHARTS: Area data processing took %ss", data_proc_duration))

    # ggplot creation timing
    ggplot_start <- Sys.time()
    plt <- e_plt %>%
      mutate(color_label = reorder(color_label, value)) %>%
      ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(year, value)) +
      ggplot2::geom_area(ggplot2::aes(fill = color_label)) +
      rcrea::theme_crea_new() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult=c(0, 0.1)),
                         labels = scales::comma_format(suffix=unit_suffix)) +
      ggplot2::scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
      ggplot2::labs(caption=paste0("Source: CREA analysis based on ", input$source, "."),
           y=NULL,
           x=NULL) +
      ggplot2::facet_wrap(~group, scales="free")
    ggplot_end <- Sys.time()
    ggplot_duration <- round(as.numeric(difftime(ggplot_end, ggplot_start, units = "secs")), 3)
    message(sprintf("📊 CHARTS: Area ggplot creation took %ss", ggplot_duration))
  }


  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }

  # Plotly conversion timing
  plotly_start <- Sys.time()
  plotly_plot <- if(chart_type == "barh") {
    message("📊 CHARTS: Converting barh to plotly...")
    plotly::ggplotly(plt, tooltip="text") %>%
      reverse_legend_labels()
  } else if(chart_type == "area") {
    message("📊 CHARTS: Converting area to plotly...")
    # Do not use aes(text=...) or ggplotly(tooltip="text") with geom_area: ggplot2
    # ignores text on areas and plotly drops fills. Patch hovers after conversion.
    plotly::ggplotly(plt) %>%
      reverse_legend_labels() %>%
      creaemission::fix_ggplotly_facets(
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
      ) %>%
      creaemission::patch_plotly_stacked_area_hover(unit_suffix = unit_suffix)
  }
  plotly_end <- Sys.time()
  plotly_duration <- round(as.numeric(difftime(plotly_end, plotly_start, units = "secs")), 3)
  message(sprintf("📊 CHARTS: Plotly conversion took %ss", plotly_duration))

  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  message(sprintf("📊 CHARTS: Plot rendering completed in %ss", duration))
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

  multiple = (input$region_type==creaemission::REGIONTYPE_NATIONAL)

  if(input$region_type==creaemission::REGIONTYPE_NATIONAL){
    # Get countries from source
    available_data <- source_obj$list_available_data()

    countries <- available_data %>%
      distinct(iso3) %>%
      mutate(country = creaemission::iso3_to_country(iso3)) %>%
      filter(iso3!="world", !is.na(country)) %>%
      arrange(country) %>%
      distinct(country, iso3) %>%
      tibble::deframe()
    countries <- c("All"="all", "World"="world", countries)

    # Get previously selected countries that are still available
    prev_selected <- selected_countries()
    if(!is.null(prev_selected)) {
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
      mutate(country = creaemission::iso3_to_country(iso3)) %>%
      distinct(country, iso3) %>%
      tibble::deframe()

    # Get previously selected country if still available
    prev_selected <- selected_countries()
    if(!is.null(prev_selected)) {
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

  if (input$region_type == creaemission::REGIONTYPE_PROVINCIAL) {
     names(choices) <- gsub("Country", "Province", names(choices))
  }

  selectInput("color_by", "Color by:", multiple=F, choices = choices, selected=color_bys[2])
})

output$selectGroupBy <- renderUI({
  req(input$region_type)
  choices <- group_bys

  if (input$region_type == creaemission::REGIONTYPE_PROVINCIAL) {
     names(choices) <- gsub("Country", "Province", names(group_bys))
  }

  selectInput("group_by", "Group by:", multiple=F, choices = choices, selected=group_bys[1])
})
