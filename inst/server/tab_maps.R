# Map-specific reactive values (similar to charts tab)
selected_map_source <- reactiveVal("CEDS")  # Default to CEDS
selected_map_year <- reactiveVal(NULL)
selected_map_pollutant <- reactiveVal("NOx")  # Default to NOx
selected_map_sector <- reactiveVal("Energy")  # Default to Energy (Power Generation)
selected_map_country <- reactiveVal("wld")  # Default to global view

# Performance logging function
log_performance <- function(operation, start_time, details = "") {
  elapsed <- Sys.time() - start_time
  message(glue::glue("PERFORMANCE: {operation} took {round(as.numeric(elapsed), 3)}s {details}"))
}

# Observers to update stored selections when input changes
observeEvent(input$map_source, {
  selected_map_source(input$map_source)
})

observeEvent(input$map_year, {
  selected_map_year(input$map_year)
})

observeEvent(input$map_pollutant, {
  selected_map_pollutant(input$map_pollutant)
})

observeEvent(input$map_sector, {
  selected_map_sector(input$map_sector)
})

observeEvent(input$map_country, {
  selected_map_country(input$map_country)
})

# Reactive for current map source object
current_map_source <- reactive({
  req(input$map_source)
  
  start_time <- Sys.time()
  source_obj <- get_current_source(input$map_source, "map")
  log_performance("get_current_map_source", start_time, glue::glue("source={input$map_source}"))
  
  return(source_obj)
})

# Function to validate and update map selections when source changes
validate_and_update_map_selections <- function(new_source) {
  start_time <- Sys.time()
  
  # Get current source object
  source_obj <- current_map_source()

  # Get available data from new source
  available_data_start <- Sys.time()
  available_data <- source_obj$list_available_data()
  log_performance("list_available_data (map validation)", available_data_start, "getting available data for validation")
  
  # Handle case where no data is available
  if (nrow(available_data) == 0) {
    message("Warning: No data available for map source")
    log_performance("validate_and_update_map_selections", start_time, "no data available")
    return(list(
      year = NULL,
      pollutant = "NOx",
      sector = "Energy", 
      country = "wld"
    ))
  }
  
  available_years <- sort(unique(available_data$year))
  available_pollutants <- unique(available_data$pollutant)
  available_sectors <- unique(available_data$sector)

  # Validate year selection
  current_year <- selected_map_year()
  if (!is.null(current_year) && current_year %in% available_years) {
    # Keep current year if it's available
    year_to_use <- current_year
  } else {
    # Use latest available year as default
    year_to_use <- max(available_years)
    selected_map_year(year_to_use)
  }

  # Validate pollutant selection
  current_pollutant <- selected_map_pollutant()
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
    selected_map_pollutant(pollutant_to_use)
  }
  
  # Debug output
  message(glue::glue("Map pollutant validation: current={current_pollutant}, available={paste(available_pollutants, collapse=', ')}, selected={pollutant_to_use}"))

  # Validate sector selection
  current_sector <- selected_map_sector()
  if (!is.null(current_sector) && current_sector %in% available_sectors) {
    # Keep current sector if it's available
    sector_to_use <- current_sector
  } else {
    # Use Energy (Power Generation) as default if available, otherwise first available sector
    if ("Energy" %in% available_sectors) {
      sector_to_use <- "Energy"
    } else {
      sector_to_use <- available_sectors[1]
    }
    selected_map_sector(sector_to_use)
  }
  
  # Debug output
  message(glue::glue("Map sector validation: current={current_sector}, available={paste(available_sectors, collapse=', ')}, selected={sector_to_use}"))

  # For maps, we'll use a predefined list of countries since map sources don't have country info
  # Keep current country selection if it's valid
  current_country <- selected_map_country()
  if (!is.null(current_country)) {
    country_to_use <- current_country
  } else {
    # Use "wld" as default for global view
    country_to_use <- "wld"
  }

  # Update stored selections
  selected_map_country(country_to_use)

  log_performance("validate_and_update_map_selections", start_time, glue::glue("source={new_source}, years={length(available_years)}, pollutants={length(available_pollutants)}, sectors={length(available_sectors)}"))

  return(list(
    year = year_to_use,
    pollutant = pollutant_to_use,
    sector = sector_to_use,
    country = country_to_use
  ))
}

# Observer for map source changes
observeEvent(input$map_source, {
  start_time <- Sys.time()
  
  req(input$map_source)

  # Validate and update selections
  validated_selections <- validate_and_update_map_selections(input$map_source)

  # Update UI inputs with validated selections
  updateSelectInput(session, "map_year", selected = validated_selections$year)
  updateSelectInput(session, "map_pollutant", selected = validated_selections$pollutant)
  updateSelectInput(session, "map_sector", selected = validated_selections$sector)
  updateSelectInput(session, "map_country", selected = validated_selections$country)
  
  log_performance("map source change observer", start_time, glue::glue("source={input$map_source}"))
})

# Initial setup observer - runs once when the app starts
observe({
  start_time <- Sys.time()
  
  req(input$map_source)
  
  # Only run this once when the source is first available
  if (is.null(selected_map_year())) {
    # Validate and update selections for initial setup
    validated_selections <- validate_and_update_map_selections(input$map_source)
    
    # Update UI inputs with validated selections
    updateSelectInput(session, "map_year", selected = validated_selections$year)
    updateSelectInput(session, "map_pollutant", selected = validated_selections$pollutant)
    updateSelectInput(session, "map_sector", selected = validated_selections$sector)
    updateSelectInput(session, "map_country", selected = validated_selections$country)
    
    # Debug output
    message(glue::glue("Initial map setup: pollutant={validated_selections$pollutant}, sector={validated_selections$sector}"))
    
    log_performance("initial map setup observer", start_time, glue::glue("source={input$map_source}"))
  }
})

# Download Handlers ----------------------------------
# Downloadable tif of selected dataset
output$download_map <- downloadHandler(
  filename = function() {
    paste("emissions.tif", sep = "")
  },
  content = function(file) {
    start_time <- Sys.time()
    
    raster <- emissions_raster()
    if (!is.null(raster)) {
      terra::writeRaster(raster, file, overwrite = TRUE)
      log_performance("raster download", start_time, glue::glue("pixels={terra::ncell(raster)}"))
    }
  }
)

# UI Output Elements --------------------------------------
output$map_source_select <- renderUI({
  selectInput("map_source", "Data source:",
              choices = c('CEDS'='CEDS', 'EDGAR'='EDGAR'),
              selected='CEDS',
              multiple=F)
})

output$map_pollutant_select <- renderUI({
  req(current_map_source())

  # Get current source object
  source_obj <- current_map_source()

  # Get available pollutants from source
  available_data_start <- Sys.time()
  available_data <- source_obj$list_available_data()
  log_performance("list_available_data (map pollutant UI)", available_data_start, "getting pollutants for UI")
  
  # Handle case where no data is available
  if (nrow(available_data) == 0) {
    return(selectInput("map_pollutant", "Pollutant:",
                       multiple=F,
                       choices = c("NOx" = "NOx"),
                       selected = "NOx"))
  }
  
  available_pollutants <- unique(available_data$pollutant)

  # Create choices directly from available pollutants (no global filtering)
  available_pollutants_choices <- available_pollutants
  names(available_pollutants_choices) <- available_pollutants

  # Get previously selected pollutant if it's still available
  prev_selected <- selected_map_pollutant()
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
  message(glue::glue("Map pollutant UI: prev={prev_selected}, available={paste(available_pollutants_choices, collapse=', ')}, selected={selected}"))

  selectInput("map_pollutant", "Pollutant:",
              multiple=F,
              choices = available_pollutants_choices,
              selected=selected)
})

output$map_year_select <- renderUI({
  req(current_map_source())

  # Get current source object
  source_obj <- current_map_source()

  # Get available years from actual data
  available_data_start <- Sys.time()
  available_data <- source_obj$list_available_data()
  log_performance("list_available_data (map year UI)", available_data_start, "getting years for UI")
  
  # Handle case where no data is available
  if (nrow(available_data) == 0) {
    return(selectInput("map_year", "Year:",
                       multiple=F,
                       choices = c("2022" = 2022),
                       selected = 2022))
  }
  
  years <- sort(unique(available_data$year))

  # Get previously selected year if it's still available
  prev_selected <- selected_map_year()
  if(!is.null(prev_selected) && prev_selected %in% years) {
    selected <- prev_selected
  } else {
    selected <- max(years)
  }

  selectInput("map_year", "Year:",
              multiple=F,
              choices = rev(years),
              selected=selected)
})

output$map_sector_select <- renderUI({
  req(current_map_source())

  # Get current source object
  source_obj <- current_map_source()

  # Get available sectors from source
  available_data_start <- Sys.time()
  available_data <- source_obj$list_available_data()
  log_performance("list_available_data (map sector UI)", available_data_start, "getting sectors for UI")
  
  # Handle case where no data is available
  if (nrow(available_data) == 0) {
    return(selectInput("map_sector", "Sector:",
                       multiple=F,
                       choices = c("Energy" = "Energy"),
                       selected = "Energy"))
  }
  
  available_sectors <- unique(available_data$sector)

  # Get previously selected sector if it's still available
  prev_selected <- selected_map_sector()
  if(!is.null(prev_selected) && prev_selected %in% available_sectors) {
    selected <- prev_selected
  } else {
    # Use Energy (Power Generation) as default if available, otherwise first available sector
    if ("Energy" %in% available_sectors) {
      selected <- "Energy"
    } else {
      selected <- available_sectors[1]
    }
  }
  
  # Debug output
  message(glue::glue("Map sector UI: prev={prev_selected}, available={paste(available_sectors, collapse=', ')}, selected={selected}"))

  selectInput("map_sector", "Sector:",
              multiple=F,
              choices = available_sectors,
              selected=selected)
})

output$map_country_select <- renderUI({
  req(current_map_source())

  # For maps, use a predefined list of countries since map sources work with global data
  # and can crop to specific countries
  countries <- c("Global"="wld",
                 "Indonesia"="IDN",
                 "India"="IND",
                 "China"="CHN",
                 "Thailand"="THA",
                 "Vietnam"="VNM",
                 "South Africa"="ZAF",
                 "United States"="USA",
                 "Brazil"="BRA",
                 "Russia"="RUS")

  # Get previously selected country if it's still available
  prev_selected <- selected_map_country()
  if(!is.null(prev_selected) && prev_selected %in% countries) {
    selected <- prev_selected
  } else {
    selected <- "wld"
  }

  selectInput("map_country", "Country:",
              multiple=F,
              choices = countries,
              selected=selected)
})

# Output Elements --------------------------------------
emissions_raster <- reactive({
  req(input$map_pollutant)
  req(input$map_year)
  req(input$map_source)
  req(input$map_sector)
  req(input$map_country)

  tryCatch({
    # Get the map source object
    source_obj <- current_map_source()

    start_time <- Sys.time()
    # Get the raster from the source
    raster <- source_obj$get(
      pollutant = input$map_pollutant,
      sector = input$map_sector,
      year = input$map_year,
      iso3 = input$map_country
    )
    log_performance("raster_get", start_time, glue::glue("pollutant={input$map_pollutant}, sector={input$map_sector}, year={input$map_year}, country={input$map_country}"))

    return(raster)
  }, error = function(e) {
    message(glue::glue("Error getting emissions raster: {e$message}"))
    return(NULL)
  })
})

output$map <- renderLeaflet({
  start_time <- Sys.time()
  
  r <- emissions_raster()
  sector <- input$map_sector
  pollutant <- input$map_pollutant
  source_name <- input$map_source
  palette <- input$map_palette

  req(r)
  req(sector)
  
  # Configuration for raster visualization
  # Adjust these values based on your needs and system performance
  max_pixels <- 6.48e6    # Maximum pixels for visualization (6,480,000)
  # This limit accommodates high-resolution global rasters (e.g., 0.1° resolution = 3600x1800 = 6.48M pixels)
  resample_method <- "bilinear"  # Resampling method: "bilinear", "near", or "cubic"
  enable_resampling <- FALSE     # Disabled since we're using higher pixel limit
  
  # Map display configuration
  default_zoom <- 2        # Default zoom level (higher = more zoomed in)
  # Zoom levels: 1=world, 2=continents, 3=countries, 4=regions, 5=cities, 6=districts
  default_lng <- 0         # Default longitude (centered on prime meridian)
  default_lat <- 0         # Default latitude (centered on equator)

  # Convert to appropriate units for display
  # Original units are very small (e.g., 10^-6 kg m-2 s-1), so we multiply by 1e6
  # to make the values more readable on the map (e.g., 1.0 instead of 0.000001)
  emission <- r * 1e6  # Convert to appropriate scale
  
  # Check if raster has too many pixels for proper visualization
  # We've set a high limit (6.48M pixels) to avoid resampling and preserve data quality
  n_pixels <- terra::ncell(emission)
  
  if (n_pixels > max_pixels && enable_resampling) {
    message(glue::glue("Raster has {n_pixels} pixels, resampling to {max_pixels} for proper visualization"))
    
    # Calculate target resolution to get close to max_pixels
    target_res <- sqrt(n_pixels / max_pixels) * terra::res(emission)[1]
    
    # Resample using the configured method to preserve data quality
    # Alternative methods: "near" (nearest neighbor) or "cubic" (cubic convolution)
    emission <- terra::resample(emission, 
                               terra::rast(resolution = target_res, 
                                          extent = terra::ext(emission),
                                          crs = terra::crs(emission)),
                               method = resample_method)
        
    # Optional: Show original vs resampled resolution for transparency
    original_res <- terra::res(r)[1]
    message(glue::glue("Original resolution: {round(original_res, 6)} degrees"))
    message(glue::glue("Resampled resolution: {round(target_res, 6)} degrees"))
  } else if (n_pixels > max_pixels && !enable_resampling) {
    message(glue::glue("Warning: Raster has {n_pixels} pixels (> {format(max_pixels, scientific = TRUE)})."))
    message("This exceeds the configured limit. Consider increasing max_pixels if visualization is limited.")
  } else {
    message(glue::glue("Raster has {n_pixels} pixels, within visualization limit ({format(max_pixels, scientific = TRUE)})"))
  }
  
  # Get units from the raster object if available
  raster_units <- terra::units(r)
  
  # Create layer name with sector and units
  # This will appear in the mapview layer control box
  # Format: "Sector - Pollutant (units)"
  layer_name <- paste0(sector, " - ", pollutant, " (", raster_units, ")")
  
  # Add debug output to show what units are being used

  # Apply pole fix for global data (fix for Leaflet display issues)
  # emission <- terra::crop(emission, terra::ext(-180, 180, -89, 89))

  # Calculate breaks for visualization
  emission_values <- emission[]
  emission_values <- emission_values[!is.na(emission_values) & emission_values > 0]

  if (length(emission_values) == 0) {
    # Create empty map if no data
    log_performance("map rendering (empty)", start_time, "no data available")
    return(leaflet() %>%
      addTiles() %>%
      setView(lng = default_lng, lat = default_lat, zoom = default_zoom))
  } else {
    # Calculate breaks
    saturation <- quantile(emission_values, 0.999, na.rm = TRUE)
    breaks <- c(seq(0, saturation, length.out = 14), max(emission_values, na.rm = TRUE))

    # Round breaks to significant digits for cleaner legend display
    # This removes unnecessary decimal places while preserving meaningful precision
    legend_digits <- 2  # Change this value to control legend precision (1-3 recommended)
    breaks <- signif(breaks, digits = legend_digits)
    
    # Get colors
    colors <- hcl.colors(15, gsub("REVERSE", "", palette), rev = grepl("REVERSE", palette))

    # Create map
    mapview_start <- Sys.time()
    mapviewOptions(mapview.maxpixels = max_pixels)
    map <- mapview(emission,
                   layer.name = layer_name,
                   at = breaks,
                   col.regions = colors)
    
    log_performance("mapview creation", mapview_start, glue::glue("pixels={n_pixels}, breaks={length(breaks)}"))
    log_performance("map rendering total", start_time, glue::glue("pixels={n_pixels}, sector={sector}, pollutant={pollutant}"))
    
    return(map@map %>%
      setView(lng = default_lng, lat = default_lat, zoom = default_zoom))
  }
})
