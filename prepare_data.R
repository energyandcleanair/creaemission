
#' Read all CEDS data and extract lighter country-specific RDS files to be used in time series
#' Saves time and memory (and prevents actual crash on ShinyApps.io)
#'
#'
#' @return
#' @export
#'
#' @examples
prepare_countries_ts <- function(dest_dir="data/v2024_04_01/by_country"){


  # read all files
  years <- seq(2000, 2022)
  emissions <- lapply(years, get_emissions) %>%
    bind_rows()

  # Add world
  emissions <- emissions %>%
    bind_rows(
      emissions %>%
        group_by(poll, sector, fuel, units, year) %>%
        summarise(value=sum(value, na.rm=T)) %>%
        mutate(country="World", iso="world")
    )

  dir.create(dest_dir, showWarnings = F)

  # Split by country and save
  split(emissions, emissions$iso) %>%
    walk(~{
      iso <- .$iso[1]
      dest_file <- file.path(dest_dir, paste0(iso, ".rds"))
      saveRDS(., dest_file)
    })
}


#' Build provincial time series using raster/netCDF data
prepare_provinces_ts <- function(
    dest_dir="data/v2024_04_01/provincial",
    iso2s=c("ID", "IN", "CN"),
    years=seq(2000, 2022)
){

  dir.create(dest_dir, showWarnings = F)

  emissions <- extract_provincial_data(
    year=years,
    country_id=iso2s,
    level=1,
    res="low"
  )


  lapply(split(emissions, emissions$GID_0), function(emission_iso){
    iso3 <- tolower(emission_iso$GID_0[1])
    dest_file <- file.path(dest_dir, paste0(iso3, ".rds"))
    saveRDS(emission_iso, dest_file)
  })
}
