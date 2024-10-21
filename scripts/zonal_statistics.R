extract_provincial_data <- function(year=2022,
                                    pollutants=c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2"),
                                    country_id="ID",
                                    level=1,
                                    res="low",
                                    buffer_into_sea_km=20
                                    ){

  library(glue)
  library(ncdf4)
  library(tidyverse)


  dir <- "data/netcdf"
  emissions <- pbapply::pblapply(pollutants, function(pollutant){
    dest_file <- download_nc(year, pollutant, dir)
    extract_emission(dest_file, country_id, level, res, buffer_into_sea_km) %>%
     mutate(pollutant=pollutant,
            year=year,
            res=res)
  }) %>%
    bind_rows()

  return(emissions)
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

extract_emission <- function(file, country_id, level=1, res="full", buffer_into_sea_km=20){

  # Get geometries
  readRenviron(".Renviron")
  vect <- creahelpers::get_adm(level=level, res=res, iso2s=country_id) %>%
    terra::vect()

  if(buffer_into_sea_km > 0){
    vect <- buffer_into_sea(vect, id_col=glue("GID_{level}"), buffer_into_sea_km)
  }

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
      terra::mean() * area_r_m2
  })

  # extract zonal mean
  zonal_mean <- lapply(rs, function(r_sector){
    terra::extract(r_sector, terra::makeValid(vect), fun="sum", exact=T, ID=F)
  })

  zonal_emission_kt_per_year <- lapply(zonal_mean, function(z){
    z * 365 * 24 * 3600 / 1e6
  })

  # cbind all
  do.call(cbind, zonal_emission_kt_per_year) %>%
    as.data.frame() %>%
    setNames(sector_names) %>%
    cbind(vect[,c("GID_0", "GID_1", "NAME_1")]) %>%
    gather(key="sector_code", value="emission", -c("GID_0", "GID_1", "NAME_1")) %>%
    tibble()
}

validate_emissions <- function(emissions){

  years <- unique(emissions$year)
  national_emissions <- lapply(years, function(y) readRDS(glue("data/v2024_04_01/ceds_emissions_{y}.RDS"))) %>%
    bind_rows()

  # Check if the sum of provincial emissions is equal to national emissions
  emissions %>%
    mutate(iso=tolower(GID_0)) %>%
    rename(poll=pollutant) %>%
    inner_join(
      national_emissions %>% distinct(poll, iso),
      relationship = "many-to-many"
    ) %>%
    group_by(iso, poll, year) %>%
    summarise(emission=sum(emission)) %>%
    mutate(level="provincial") %>%
    bind_rows(
      national_emissions %>%
        select(iso, poll, year, emission=value) %>%
        mutate(level="national") %>%
        inner_join(
          emissions %>% distinct(poll=pollutant, iso=tolower(GID_0)),
          relationship = "many-to-many"
        ) %>%
        group_by(iso, poll, level, year) %>%
        summarise(emission=sum(emission))
    ) %>%
    ungroup() %>%
    spread(key="level", value="emission")
}

buffer_into_sea <- function(vect, id_col, buffer_km=20)
{

  # Prepare
  g_sf <- vect %>%
    sf::st_as_sf() %>%
    sf::st_transform(3857) %>%
    sf::st_make_valid()

  # Get coastal buffer
  g_coast <- cartomisc::regional_seas(
    g_sf,
    group = id_col,
    dist = buffer_km * 1000)

  # Join back and reproject
  g_combined <- bind_rows(g_sf, g_coast) %>%
    dplyr::group_by_at(id_col) %>%
    summarise() %>%
    sf::st_transform(sf::st_crs(sf::st_as_sf(vect))) %>%
    sf::st_make_valid() %>%
    filter(!is.na(!!rlang::sym(id_col))) %>%
    left_join(g_sf %>%
                as.data.frame() %>%
                dplyr::select(-geometry),
              by = id_col) %>%
    terra::vect() %>%
    terra::makeValid()

  return(g_combined)
}
