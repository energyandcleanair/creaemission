library(mongolite)
library(dbplyr)
library(tibble)
library(tidyverse)
library(countrycode)
library(colorRamps)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(lubridate)
library(scales)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras2)
library(plotly)
library(DT)
library(scales)
library(raster)
library(mapview)
library(glue)
sel <- dplyr::select

# Files are now in the same directory and will be loaded by the package
# No need to source them explicitly

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

chart_types <- c("Bar (Horizontal)"="barh",
                 "Timeseries (area)"="area")
# topn <- 20 # How many rows max in chart

# Use CEDS provincial sector mappings from the new sector_mappings.R file
ceds_sectors <- CEDS_PROVINCIAL_SECTORS

map_palettes=c("Viridis"="viridis","OrRd"="OrRdREVERSE","Inferno"="inferno")

# Global sources list for managing source objects
sources <- list(
  national = list(
    ceds = CEDSNational$new(),
    edgar = EDGARNational$new()
  ),
  provincial = list(
    ceds = CEDSProvincial$new(),
    edgar = EDGARProvincial$new()
  )
)

# Helper function to get current source object
get_current_source <- function(source_name, region_type) {
  source_name_lower <- tolower(source_name)
  region_type_lower <- tolower(region_type)

  if (region_type_lower == "national") {
    return(sources$national[[source_name_lower]])
  } else if (region_type_lower == "provincial") {
    return(sources$provincial[[source_name_lower]])
  } else {
    stop(glue::glue("Invalid region_type: {region_type}"))
  }
}

