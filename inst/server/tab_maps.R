# Map-specific reactive values (similar to charts tab)
selected_map_source <- reactiveVal(NULL)
selected_map_year <- reactiveVal(NULL)
selected_map_pollutant <- reactiveVal(NULL)
selected_map_sector <- reactiveVal(NULL)
selected_map_country <- reactiveVal(NULL)

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
  get_current_source(input$map_source, "map")
})

# Function to validate and update map selections when source changes
validate_and_update_map_selections <- function(new_source) {
  # Get current source object
  source_obj <- current_map_source()

  # Get available data from new source
  available_data <- source_obj$list_available_data()
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
    # Use first available pollutant as default
    pollutant_to_use <- available_pollutants[1]
    selected_map_pollutant(pollutant_to_use)
  }

  # Validate sector selection
  current_sector <- selected_map_sector()
  if (!is.null(current_sector) && current_sector %in% available_sectors) {
    # Keep current sector if it's available
    sector_to_use <- current_sector
  } else {
    # Use first available sector as default
    sector_to_use <- available_sectors[1]
    selected_map_sector(sector_to_use)
  }

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
  available_pollutants <- unique(available_data$pollutant)

  # Create choices directly from available pollutants (no global filtering)
  available_pollutants_choices <- available_pollutants
  names(available_pollutants_choices) <- available_pollutants

  # Get previously selected pollutant if it's still available
  prev_selected <- selected_map_pollutant()
  if(!is.null(prev_selected) && prev_selected %in% available_pollutants_choices) {
    selected <- prev_selected
  } else {
    selected <- available_pollutants_choices[1]
  }

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
  available_sectors <- unique(available_data$sector)

  # Get previously selected sector if it's still available
  prev_selected <- selected_map_sector()
  if(!is.null(prev_selected) && prev_selected %in% available_sectors) {
    selected <- prev_selected
  } else {
    selected <- available_sectors[1]
  }

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


    # Get the raster from the source
    raster <- source_obj$get(
      pollutant = input$map_pollutant,
      sector = input$map_sector,
      year = input$map_year,
      iso3 = input$map_country
    )

    return(raster)
  }, error = function(e) {
    message(glue::glue("Error getting emissions raster: {e$message}"))
    return(NULL)
  })
})

output$map <- renderLeaflet({
  r <- emissions_raster()
  sector <- input$map_sector
  palette <- input$map_palette

  req(r)
  req(sector)

  # Convert to appropriate units for display
  emission <- r * 1e6  # Convert to appropriate scale

  # Apply pole fix for global data (fix for Leaflet display issues)
  # emission <- terra::crop(emission, terra::ext(-180, 180, -89, 89))

  # Calculate breaks for visualization
  emission_values <- emission[]
  emission_values <- emission_values[!is.na(emission_values) & emission_values > 0]

  if (length(emission_values) == 0) {
    # Create empty map if no data
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  } else {
    # Calculate breaks
    saturation <- quantile(emission_values, 0.999, na.rm = TRUE)
    breaks <- c(seq(0, saturation, length.out = 14), max(emission_values, na.rm = TRUE))

    # Get colors
    colors <- hcl.colors(15, gsub("REVERSE", "", palette), rev = grepl("REVERSE", palette))

    # Create map
    map <- mapview(emission,
                   layer.name = sector,
                   at = breaks,
                   col.regions = colors)
    map@map
  }
})
