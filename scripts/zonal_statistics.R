




extract_provincial_data <- function(year=2022,
                                    pollutants=c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2"),
                                    country_id="ID",
                                    level=1,
                                    res="low"
                                    ){

  library(glue)
  library(ncdf4)
  library(tidyverse)

  dir <- "data/netcdf"
  emissions <- pbapply::pblapply(pollutants, function(pollutant){
    dest_file <- download_nc(year, pollutant, dir)
    extract_emission(dest_file, country_id, level, res) %>%
     mutate(pollutant=pollutant,
            year=year)
  }) %>%
    bind_rows()

}

download_nc <- function(year, pollutant, dir){

  dir.create(dir, showWarnings = FALSE)
  url <- glue("https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/{pollutant}/{pollutant}-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-07-08_gr_{year}01-{year}12.nc")
  dest_file <- glue("{dir}/{pollutant}_{year}.nc")
  if(file.exists(dest_file) & file.info(dest_file)$size > 1.6e8){
    return(dest_file)
  }

  # try downloading 5 times
  for(i in 1:10){
    tryCatch({
      download.file(url, dest_file)
      break
    }, error = function(e){
      message(glue("Error downloading {url}"))
      file.remove(dest_file)
      Sys.sleep(5)
    })
  }

  return(dest_file)
}

extract_emission <- function(file, country_id, level=1, res="full"){

  # Get geometries
  readRenviron(".Renviron")
  vect <- creahelpers::get_adm(level=level, res=res, iso2s=country_id) %>%
    terra::vect()

  # Extract the emission data
  r <- terra::rast(file)
  sector_codes <- terra::varnames(r)
  sector_names <- terra::longnames(r)
  unit <- unique(terra::units(r))
  if(unit != "kg m-2 s-1") stop("Unit is not kg m-2 s-1")

  area_r_m2 <- terra::cellSize(r, unit="m")

  rs <- lapply(sector_codes, function(sector_code){
    months <- 1:12
    terra::subset(r, glue("{sector_code}_{months}")) %>%
      terra::mean() * area_r_km2
  })

  # extract zonal mean
  zonal_mean <- lapply(rs, function(r_sector){
    terra::extract(r_sector, vect, fun="sum", exact=T, ID=F)
  })

  zonal_emission_kt_per_year <- lapply(zonal_mean, function(z){
    z * 365 * 24 * 3600 / 1e6
  })

  # cbind all
  do.call(cbind, zonal_emission_kt_per_year) %>%
    as.data.frame() %>%
    setNames(sector_names) %>%
    cbind(vect[,c("GID_0", "NAME_1")]) %>%
    gather(key="sector_code", value="emission", -c("GID_0","NAME_1"))
}

validate_emissions <- function(emissions, year){

  national_emissions <- readRDS(glue("data/v2024_04_01/ceds_emissions_{year}.RDS"))


  # Check if the sum of provincial emissions is equal to national emissions
  emissions %>%
    mutate(iso=tolower(GID_0)) %>%
    rename(poll=pollutant) %>%
    inner_join(
      national_emissions %>% distinct(poll, iso),
      relationship = "many-to-many"
    ) %>%
    group_by(iso, poll) %>%
    summarise(emission=sum(emission)) %>%
    mutate(level="provincial") %>%
    bind_rows(
      national_emissions %>%
        select(iso, poll, emission=value) %>%
        mutate(level="national") %>%
        inner_join(
          prov_emissions %>% distinct(poll=pollutant, iso=tolower(GID_0)),
          relationship = "many-to-many"
        ) %>%
        group_by(iso, poll, level) %>%
        summarise(emission=sum(emission))
    ) %>%
    ungroup() %>%
    spread(key="level", value="emission")


  # Check if the sum of provincial emissions is equal to national emissions
  emissions %>%
    mutate(iso=tolower(GID_0)) %>%
    rename(poll=pollutant) %>%
    inner_join(
      national_emissions %>% distinct(poll, iso),
      relationship = "many-to-many"
    ) %>%
    group_by(iso, poll, sector=sector_code) %>%
    summarise(emission=sum(emission)) %>%
    mutate(level="provincial") %>%
    bind_rows(
      national_emissions %>%
        select(iso, poll, emission=value, sector) %>%
        mutate(level="national") %>%
        inner_join(
          prov_emissions %>% distinct(poll=pollutant, iso=tolower(GID_0)),
          relationship = "many-to-many"
        ) %>%
        group_by(iso, poll, level, sector) %>%
        summarise(emission=sum(emission))
    ) %>%
    ungroup() %>%
    ggplot() +
    geom_col(aes(x=level, y=emission, fill=sector), show.legend = F) +
    facet_wrap(~poll) -> plt

  library(plotly)
  ggplotly(plt)


}
