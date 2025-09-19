library(dplyr)

# Pollutants are now derived dynamically from available_data
# No global pollutants variable needed
color_bys <- c("Country"="region_name", "Sector" = "sector_group", "Sector - Detailed"="sector", "Fuel"="fuel")
group_bys <- c("Country"="region_name", "Sector" = "sector_group", "Sector - Detailed"="sector", "Fuel"="fuel")

chart_types <- c("Bar (Horizontal)"="barh",
                 "Timeseries (area)"="area")
map_palettes=c("Viridis"="viridis","OrRd"="OrRdREVERSE","Inferno"="inferno")


# Lazy source initialization (defer heavy package loads until first use)
sources_env <- new.env(parent = emptyenv())
sources_env$national <- new.env(parent = emptyenv())
sources_env$provincial <- new.env(parent = emptyenv())
sources_env$map <- new.env(parent = emptyenv())

create_source <- function(region_type_lower, source_name_lower) {
  src <- NULL
  if (region_type_lower == "national") {
    if (source_name_lower == "ceds") src <- creaemission::CEDSNational$new()
    if (source_name_lower == "edgar") src <- creaemission::EDGARNational$new()
  } else if (region_type_lower == "provincial") {
    if (source_name_lower == "ceds") src <- creaemission::CEDSProvincial$new()
    if (source_name_lower == "edgar") src <- creaemission::EDGARProvincial$new()
  } else if (region_type_lower == "map") {
    if (source_name_lower == "ceds") src <- creaemission::CEDSMap$new()
    if (source_name_lower == "edgar") src <- creaemission::EDGARMap$new()
  }
  if (is.null(src)) stop(glue::glue("Invalid source/region combination: {source_name_lower}/{region_type_lower}"))
  src
}

# Helper function to get current source object (lazily creates and caches)
get_current_source <- function(source_name, region_type) {
  source_name_lower <- tolower(source_name)
  region_type_lower <- tolower(region_type)

  bucket <- switch(region_type_lower,
                   national = sources_env$national,
                   provincial = sources_env$provincial,
                   map = sources_env$map,
                   stop(glue::glue("Invalid region_type: {region_type}")))

  if (!exists(source_name_lower, envir = bucket, inherits = FALSE)) {
    assign(source_name_lower, create_source(region_type_lower, source_name_lower), envir = bucket)
  }
  get(source_name_lower, envir = bucket, inherits = FALSE)
}
