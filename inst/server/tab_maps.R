# Map-specific reactive values (similar to charts tab)
selected_map_source <- reactiveVal("CEDS")  # Default to CEDS
selected_map_year <- reactiveVal(NULL)
selected_map_pollutant <- reactiveVal("NOx")  # Default to NOx
selected_map_sector <- reactiveVal("Energy")  # Default to Energy (Power Generation)
selected_map_country <- reactiveVal("wld")  # Default to global view

# Note: viridisLite is now imported via DESCRIPTION dependencies


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
  
  source_obj <- get_current_source(input$map_source, "map")
  return(source_obj)
})

# Function to validate and update map selections when source changes
validate_and_update_map_selections <- function(new_source) {
  # Get current source object
  source_obj <- current_map_source()

  # Get available data from new source
  available_data <- source_obj$list_available_data()
  
  # Handle case where no data is available
  if (nrow(available_data) == 0) {
    message("Warning: No data available for map source")
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

  return(list(
    year = year_to_use,
    pollutant = pollutant_to_use,
    sector = sector_to_use,
    country = country_to_use
  ))
}

# Observer for map source changes
observeEvent(input$map_source, {
  req(input$map_source)

  # Validate and update selections
  validated_selections <- validate_and_update_map_selections(input$map_source)

  # Update UI inputs with validated selections
  updateSelectInput(session, "map_year", selected = validated_selections$year)
  updateSelectInput(session, "map_pollutant", selected = validated_selections$pollutant)
  updateSelectInput(session, "map_sector", selected = validated_selections$sector)
  updateSelectInput(session, "map_country", selected = validated_selections$country)
})

# Initial setup observer - runs once when the app starts
observe({
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
  }
})

# Download Handlers ----------------------------------
# Downloadable tif of selected dataset
output$download_map <- downloadHandler(
  filename = function() {
    paste("emissions.tif", sep = "")
  },
  content = function(file) {
    raster <- emissions_raster()
    if (!is.null(raster)) {
      terra::writeRaster(raster, file, overwrite = TRUE)
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
  available_data <- source_obj$list_available_data()
  
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
  available_data <- source_obj$list_available_data()
  
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
  available_data <- source_obj$list_available_data()
  
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
    # Try COG first, fallback to NetCDF
    raster <- source_obj$get_cog(
      pollutant = input$map_pollutant,
      sector = input$map_sector,
      year = input$map_year,
      iso3 = input$map_country,
      prefer_cog = TRUE
    )

    return(raster)
  }, error = function(e) {
    message(glue::glue("Error getting emissions raster: {e$message}"))
    return(NULL)
  })
})

# Initialize base map (runs once)
output$map <- renderLeaflet({
  # Create base map that will be updated via proxy
  leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2) %>%
    addLayersControl(
      overlayGroups = "Emissions",
      options = layersControlOptions(collapsed = FALSE)
    )
})

# Reactive values for map state
map_raster_data <- reactiveVal(NULL)
map_value_range <- reactiveVal(c(0, 1))

# Function to check TiTiler availability
check_titiler <- function() {
  tryCatch({
    if (!requireNamespace("httr", quietly = TRUE)) return(FALSE)
    response <- httr::GET("http://localhost:8000", httr::timeout(2))
    httr::status_code(response) == 200
  }, error = function(e) {
    FALSE
  })
}

# Load and process raster data
observe({
  req(input$map_pollutant, input$map_year, input$map_source, input$map_sector, input$map_country)
  
  # Get raster from emissions_raster function
  r <- emissions_raster()
  req(r)

  # Get metadata
  raster_units <- terra::units(r)
  sector <- input$map_sector
  pollutant <- input$map_pollutant

  # Apply scaling factor for better visualization
  scale_factor <- 1e6
  emission <- r * scale_factor
  
  # Apply pole fix
  emission <- terra::crop(emission, terra::ext(-180, 180, -89, 89))
  
  # Extract values for rescale calculation - use RAW values (not scaled)
  # TiTiler needs rescale in original units
  raw_values <- r[]
  raw_values <- raw_values[!is.na(raw_values) & raw_values > 0]

  # Extract scaled values for legend display
  emission_values <- emission[]
  emission_values <- emission_values[!is.na(emission_values) & emission_values > 0]

  if (length(raw_values) > 0) {
    # Always use auto-rescale (no toggle in UI)
    raw_min <- quantile(raw_values, 0.01, na.rm = TRUE)
    raw_max <- quantile(raw_values, 0.999, na.rm = TRUE)

    # Convert to scaled values for legend display
    scaled_min <- raw_min * scale_factor
    scaled_max <- raw_max * scale_factor

    # Store both raw (for TiTiler) and scaled (for legend) ranges
    map_value_range(list(
      raw = c(raw_min, raw_max),
      scaled = c(scaled_min, scaled_max)
    ))
  } else {
    # Default values when no valid data
    map_value_range(list(
      raw = c(0, 1),
      scaled = c(0, 1e6)
    ))
  }

  # Store processed raster with scale factor
  map_raster_data(list(
    raster = emission,
    values = emission_values,
    raw_values = raw_values,
    units = raster_units,
    sector = sector,
    pollutant = pollutant,
    scale_factor = scale_factor
  ))
})

# Update map visualization
observe({
  data <- map_raster_data()
  req(data, input$map_colormap)

  # Get current rescale values (separate raw and scaled)
  value_ranges <- map_value_range()
  raw_min <- value_ranges$raw[1]
  raw_max <- value_ranges$raw[2]
  scaled_min <- value_ranges$scaled[1]
  scaled_max <- value_ranges$scaled[2]

  # Generate color palette (needed for both TiTiler and fallback)
  colormap_name <- input$map_colormap
  n_colors <- 256

  # Get colors from R's built-in palettes (matching TiTiler names)
  if (colormap_name == "viridis") {
    colors <- viridisLite::viridis(n_colors)
  } else if (colormap_name == "plasma") {
    colors <- viridisLite::plasma(n_colors)
  } else if (colormap_name == "inferno") {
    colors <- viridisLite::inferno(n_colors)
  } else if (colormap_name == "magma") {
    colors <- viridisLite::magma(n_colors)
  } else if (colormap_name == "cividis") {
    colors <- viridisLite::cividis(n_colors)
  } else {
    colors <- viridisLite::viridis(n_colors)
  }

  # Get COG path for TiTiler
  source_obj <- current_map_source()
  cog_path <- source_obj$get_cog_path(data$pollutant, data$sector, input$map_year, input$map_country)

  # Decide on rendering method
  if (file.exists(cog_path) && check_titiler()) {
        # Use TiTiler for fast tile-based rendering
        message("✅ Using TiTiler for fast tile rendering")
        
    # Convert to container path
        relative_path <- gsub("^.*/creaemission/", "", cog_path)
        container_path <- paste0("/data/", relative_path)
        
    # Get colormap name
    colormap_name <- input$map_colormap

    resampling <- "nearest"
        
        tile_url <- sprintf(
      "http://localhost:8000/cog/tiles/WebMercatorQuad/{z}/{x}/{y}?url=%s&rescale=%.6e,%.6e&colormap_name=%s&resampling_method=%s",
          container_path,
      raw_min,
      raw_max,
      colormap_name,
      resampling
    )

    message(sprintf("TiTiler rescale: %.6e to %.6e (raw units)", raw_min, raw_max))
    message(sprintf("Legend range: %.6e to %.6e (scaled units)", scaled_min, scaled_max))

    # Clean layer ID
    clean_layer_id <- gsub("[^A-Za-z0-9_]", "_", paste0(data$pollutant, "_", data$sector, "_", input$map_year, "_emissions"))

    # Add TiTiler tiles
    map_proxy <- leafletProxy("map") %>%
          clearImages() %>%
          clearControls() %>%
          addTiles(
            urlTemplate = tile_url,
            options = tileOptions(
              tms = FALSE,
              crossOrigin = TRUE,
              opacity = 0.8
            ),
        attribution = paste0("TiTiler: ", data$pollutant, " (", data$units, ")"),
            layerId = clean_layer_id,
            group = "Emissions"
      )

    # Always show legend (no toggle)
    # Validate scaled values to prevent legend errors
    if (!is.na(scaled_min) && !is.na(scaled_max) && is.finite(scaled_min) && is.finite(scaled_max) && scaled_min < scaled_max) {
      # Create simple legend with manual labels to avoid formatting issues
      legend_values <- c(scaled_min,
                        scaled_min + (scaled_max - scaled_min) * 0.25,
                        scaled_min + (scaled_max - scaled_min) * 0.5,
                        scaled_min + (scaled_max - scaled_min) * 0.75,
                        scaled_max)

      # Create manual labels
      legend_labels <- c(
        sprintf("%.1e", scaled_min),
        sprintf("%.1e", legend_values[2]),
        sprintf("%.1e", legend_values[3]),
        sprintf("%.1e", legend_values[4]),
        sprintf("%.1e", scaled_max)
      )

      map_proxy <- map_proxy %>%
          addLegend(
            position = "bottomright",
          colors = colors[seq(1, 256, length.out = 5)],
          labels = legend_labels,
          title = sprintf("%s (kg/m²/yr)", data$pollutant),
          opacity = 0.8
        )
      } else {
        message("WARNING: Invalid scaled values for legend - skipping legend")
      }

    map_proxy
          
      } else {
    # Fallback to direct raster rendering
    message(if(file.exists(cog_path)) "⚠️ TiTiler not available" else "📁 COG not found", ", using direct raster rendering")

    # Create color palette function using SCALED values for consistency
    pal <- colorBin(
      palette = colors,
      domain = data$values,
      bins = seq(scaled_min, scaled_max, length.out = 256),
      na.color = "transparent"
    )

    # Clean layer ID
    clean_layer_id <- gsub("[^A-Za-z0-9_]", "_", paste0(data$pollutant, "_", data$sector, "_", input$map_year, "_emissions_fallback"))

    # Add raster image
    map_proxy <- leafletProxy("map") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(
        data$raster,
            colors = pal,
            opacity = 0.8,
            group = "Emissions",
            layerId = clean_layer_id
      )

    # Always show legend (no toggle)
    # Validate values before creating legend
    valid_values <- data$values[!is.na(data$values) & is.finite(data$values)]
    if (length(valid_values) > 0 && length(unique(valid_values)) > 1) {
      # Create simple legend for fallback to avoid formatting issues
      legend_values <- quantile(valid_values, probs = seq(0, 1, 0.25), na.rm = TRUE)
      legend_labels <- sapply(legend_values, function(x) {
        if (is.na(x) || !is.finite(x)) "N/A"
        else if (abs(x) < 1e-10) "0"
        else sprintf("%.1e", x)
      })

      # Get corresponding colors from the palette
      legend_colors <- pal(legend_values)

      map_proxy <- map_proxy %>%
        addLegend(
          position = "bottomright",
          colors = legend_colors,
          labels = legend_labels,
          title = sprintf("%s (kg/m²/yr)", data$pollutant),
          opacity = 0.8
        )
      } else {
        message("WARNING: Insufficient valid values for fallback legend - skipping legend")
      }
  }

})

# HTML Color legend generation
output$map_legend_ui <- renderUI({
  data <- map_raster_data()
  req(data, input$map_colormap)

  # Get current rescale values - use SCALED values for legend display
  value_ranges <- map_value_range()
  rescale_min <- value_ranges$scaled[1]
  rescale_max <- value_ranges$scaled[2]

  # Generate color palette matching TiTiler
  colormap_name <- input$map_colormap
  n_colors <- 256

  # Get colors from R's built-in palettes (matching TiTiler names)
  if (colormap_name == "viridis") {
    colors <- viridisLite::viridis(n_colors)
  } else if (colormap_name == "plasma") {
    colors <- viridisLite::plasma(n_colors)
  } else if (colormap_name == "inferno") {
    colors <- viridisLite::inferno(n_colors)
  } else if (colormap_name == "magma") {
    colors <- viridisLite::magma(n_colors)
  } else if (colormap_name == "cividis") {
    colors <- viridisLite::cividis(n_colors)
  } else {
    colors <- viridisLite::viridis(n_colors)
  }

  # Create colorbar HTML
  colorbar_html <- paste0(
    '<div style="width: 100%; height: 200px; position: relative;">',
    '<div style="width: 30px; height: 150px; background: linear-gradient(to top, ',
    paste(colors, collapse = ", "),
    '); border: 1px solid #666; margin: 10px auto;"></div>',
    '<div style="position: absolute; right: 10px; top: 10px; height: 150px; display: flex; flex-direction: column; justify-content: space-between; font-size: 11px; color: #333;">'
  )

  # Add value labels
  value_ticks <- seq(rescale_min, rescale_max, length.out = 6)
  for (i in 6:1) {  # Reverse order for top-to-bottom
    colorbar_html <- paste0(colorbar_html,
      '<div>', sprintf("%.1e", value_ticks[i]), '</div>')
  }

  colorbar_html <- paste0(colorbar_html, '</div>')

  # Add title
  title_text <- sprintf("%s Emissions (kg/m²/yr)", data$pollutant)

  colorbar_html <- paste0(colorbar_html,
    '<div style="text-align: center; font-weight: bold; font-size: 12px; margin-top: 5px;">',
    title_text, '</div>',
    '<div style="text-align: center; font-size: 10px; color: #666; margin-top: 2px;">',
    sprintf("Colormap: %s", input$map_colormap), '</div>',
    '</div>'
  )

  HTML(colorbar_html)
})
