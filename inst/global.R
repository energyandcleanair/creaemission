cat("[global] Loading tibble/tidyverse...\n"); flush.console()
library(dplyr)
library(RColorBrewer)
library(shinycssloaders)
cat("[global] Finished loading libraries.\n"); flush.console()
sel <- dplyr::select

# Pollutants are now derived dynamically from available_data
# No global pollutants variable needed
color_bys <- c("Country"="region_name", "Sector" = "sector_group", "Sector - Detailed"="sector", "Fuel"="fuel")
group_bys <- c("Country"="region_name", "Sector" = "sector_group", "Sector - Detailed"="sector", "Fuel"="fuel")

chart_types <- c("Bar (Horizontal)"="barh",
                 "Timeseries (area)"="area")
map_palettes=c("Viridis"="viridis","OrRd"="OrRdREVERSE","Inferno"="inferno")


# Global sources list for managing source objects
message("==== INITIALIZING DATA SOURCES ====")

sources <- list(
  national = list(
    ceds = creaemission::CEDSNational$new(),
    edgar = creaemission::EDGARNational$new()
  ),
  provincial = list(
    ceds = creaemission::CEDSProvincial$new(),
    edgar = creaemission::EDGARProvincial$new()
  ),
  map = list(
    ceds = creaemission::CEDSMap$new(),
    edgar = creaemission::EDGARMap$new()
  )
)

message("==== DATA SOURCES READY ====")

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
