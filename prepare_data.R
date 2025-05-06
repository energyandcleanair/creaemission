#' Read all emissions data and extract lighter country-specific RDS files to be used in time series
#' Saves time and memory (and prevents actual crash on ShinyApps.io)
#'
#' @param source Data source (CEDS or EDGAR)
#' @param dest_dir Destination directory for the output files
#' @param years Years to process
#'
#' @return
#' @export
#'
#' @examples
prepare_countries_ts <- function(
  source = "CEDS",
  dest_dir = NULL,
  years = seq(2000, 2022)
){
  source <- toupper(source)
  source_dir <- tolower(source)
  
  if(is.null(dest_dir)) {
    dest_dir <- glue("data/{source_dir}/national")
  }
  
  # read all files
  emissions <- lapply(years, function(y) get_emissions_national_by_year(year = y, source = source)) %>%
    bind_rows()

  # Add world
  emissions <- emissions %>%
    bind_rows(
      emissions %>%
        group_by(poll, sector, fuel, units, year) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        mutate(country = "World", iso = "world", source = source)
    )

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Split by country and save
  split(emissions, emissions$iso) %>%
    walk(~{
      iso <- .$iso[1]
      dest_file <- file.path(dest_dir, paste0(iso, ".rds"))
      saveRDS(., dest_file)
    })
}


#' Build provincial time series using raster/netCDF data
#'
#' @param source Data source (CEDS or EDGAR)
#' @param dest_dir Destination directory for the output files
#' @param iso2s ISO2 country codes to process
#' @param years Years to process
#'
#' @return
#' @export
#'
#' @examples
prepare_provinces_ts <- function(
    source = "CEDS",
    dest_dir = NULL,
    iso2s = c("ID", "IN", "CN", "ZA"),
    years = NULL
){
  source <- toupper(source)
  source_dir <- tolower(source)
  
  if(is.null(dest_dir)) {
    dest_dir <- glue("data/{source_dir}/provincial")
  }
  
  if(is.null(years)) {
    if(source == "CEDS") {
      years <- seq(2000, 2022)
    } else if(source == "EDGAR") {
      years <- seq(2000, 2022)
    }
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  emissions <- extract_provincial_data(
    year = years,
    iso2s = iso2s,
    level = 1,
    res = "low",
    source = source
  )

  lapply(split(emissions, emissions$GID_0), function(emission_iso){
    iso3 <- tolower(emission_iso$GID_0[1])
    dest_file <- file.path(dest_dir, paste0(iso3, ".rds"))
    saveRDS(emission_iso, dest_file)
  })
}