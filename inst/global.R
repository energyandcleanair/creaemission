library(creaemission)
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
library(plotly)
library(DT)
library(scales)
library(raster)
library(mapview)
library(glue)
sel <- dplyr::select

# DEBUG prints for paths
message(glue("==== PROJECT ROOT: {get_project_root()}"))
message(glue("==== PROJECT DATA: {get_data_path()}"))
message(glue("==== CURRENT FOLDER: {getwd()}"))
message(glue("{c('==== FILES IN CURRENT FOLDER:\n', paste(list.files(), collapse='\n'))}"))
message(glue("{c('==== FILES IN PARENT FOLDER:\n', paste(list.files('..'), collapse='\n'))}"))


# Files are now in the same directory and will be loaded by the package
# No need to source them explicitly

# Pollutants are now derived dynamically from available_data
# No global pollutants variable needed
color_bys <- c("Country"="region_name", "Sector" = "sector_group", "Sector - Detailed"="sector", "Fuel"="fuel")
group_bys <- c("Country"="region_name", "Sector" = "sector_group", "Sector - Detailed"="sector", "Fuel"="fuel")

chart_types <- c("Bar (Horizontal)"="barh",
                 "Timeseries (area)"="area")
# topn <- 20 # How many rows max in chart

map_palettes=c("Viridis"="viridis","OrRd"="OrRdREVERSE","Inferno"="inferno")


# Global sources list for managing source objects
message("==== INITIALIZING DATA SOURCES ====")

sources <- list(
  national = list(
    ceds = CEDSNational$new(),
    edgar = EDGARNational$new()
  ),
  provincial = list(
    ceds = CEDSProvincial$new(),
    edgar = EDGARProvincial$new()
  ),
  map = list(
    ceds = CEDSMap$new(),
    edgar = EDGARMap$new()
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
  } else if (region_type_lower == "map") {
    return(sources$map[[source_name_lower]])
  } else {
    stop(glue::glue("Invalid region_type: {region_type}"))
  }
}

