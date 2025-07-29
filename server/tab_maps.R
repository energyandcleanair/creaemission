# Download Handlers ----------------------------------
# Downloadable csv of selected dataset
output$download_map <- downloadHandler(
  filename = function() {
    paste("emissions.tif", sep = "")
  },
  content = function(file) {
    write.csv(emissions_raster(), file, row.names = FALSE)
  }
)


# Output Elements --------------------------------------
emissions_raster <- reactive({
  req(input$map_pollutant)
  req(input$map_year)
  req(input$map_source)
  req(input$map_sector)

  get_emissions_raster(
    poll=input$map_pollutant,
    year=input$map_year,
    sector=input$map_sector,
    source=input$map_source)
})


output$map <- renderLeaflet({
  r <- emissions_raster()
  sector <- input$map_sector
  palette <- input$map_palette

  req(r)
  req(sector)

  emission <- r[[sector]] * 1e12

  # Saturage for
  saturation <- quantile(emission, 0.999)
  breaks <- round(c(seq(-1e-10, saturation, length.out = 15), max(emission[])))
  breaks[1]=-1e-10 #to remove some gray bands

  colors <- hcl.colors(15, gsub("REVERSE","",palette), rev = grepl("REVERSE",palette))

  (map <- mapview(emission,
                  layer.name=names(ceds_sectors)[which(ceds_sectors==sector)],
                 at=breaks,
                 col.regions=colors))
  map@map
})
