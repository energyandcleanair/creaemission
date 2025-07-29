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


# Output Elements --------------------------------------
emissions_raster <- reactive({
  req(input$map_pollutant)
  req(input$map_year)
  req(input$map_source)
  req(input$map_sector)
  req(input$map_country)

  tryCatch({
    get_emissions_raster(
      poll = input$map_pollutant,
      year = input$map_year,
      sector = input$map_sector,
      source = input$map_source,
      iso2 = input$map_country
    )
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
    breaks <- c(0, seq(0, saturation, length.out = 14), max(emission_values, na.rm = TRUE))
    
    # Get colors
    colors <- hcl.colors(15, gsub("REVERSE", "", palette), rev = grepl("REVERSE", palette))
    
    # Create map
    map <- mapview(emission,
                   layer.name = get_sector_name(sector, input$map_source, "provincial"),
                   at = breaks,
                   col.regions = colors)
    map@map
  }
})
