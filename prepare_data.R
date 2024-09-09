
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
