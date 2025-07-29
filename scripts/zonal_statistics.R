extract_provincial_data <- function(years=seq(2000,2022),
                                    pollutants=c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2"),
                                    iso2s=c("ID","IN","CN", "TH", "VN", "ZA"),
                                    level=1,
                                    res="low",
                                    buffer_into_sea_km=20
                                    ){

  library(glue)
  library(ncdf4)
  library(tidyverse)
  library(terra)


  dir_netcdf <- "cache/netcdf"

  # While the functions could work with several iso2s in one go,
  # it can reach memory / HDD limits if so
  emissions <- lapply(iso2s, function(iso2){
    iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
    filepath <- glue("data/v2024_04_01/provincial/{tolower(iso3)}.rds")
    if(file.exists(filepath)) return(readRDS(filepath))
    message(glue("Extracting emissions for {iso2}"))
    vect <- get_vect(iso2, res, level, buffer_into_sea_km)
    stack <- get_stack(vect, years, pollutants, dir_netcdf)
    result <- extract_emission(vect, stack, iso2, level, res)
    saveRDS(result, filepath)
    return(result)
  }) %>%
    bind_rows()


  validation <- validate_emissions(emissions, include_shipping=TRUE)
  View(validation)

  return(emissions)
}


get_vect <- function(iso2s, res, level, buffer_into_sea_km){
  vect <- creahelpers::get_adm(level=level, res=res, iso2s=iso2s) %>%
    terra::vect()

  if(buffer_into_sea_km > 0){
    message("Extending provinces into sea")
    vect <- buffer_into_sea(vect, id_col=glue("GID_{level}"), buffer_into_sea_km)
  }
  return(vect)
}


#' Take a rast from a CEDS file and average it by year/sector
#'
#' @param r
#'
#' @return
#' @export
#'
#' @examples
average_by_year <- function(r){
  indexes <- rep(1:(terra::nlyr(r)/12), each=12)
  lapply(terra::split(r, indexes),
                             function(x){
                               name <- names(x)[1]
                               # remove month from name (or or two digits)
                               name <- str_remove(name, "_\\d{1,2}")
                               terra::mean(x) %>%
                                 `names<-`(name)
                             }) %>%
    terra::rast()
}

get_stack <- function(vect, years, pollutants, dir){

  files <- tidyr::crossing(year=years, pollutant=pollutants) %>%
    rowwise() %>%
    mutate(file=download_nc(year, pollutant, dir))

  stack <- pbapply::pbmapply(
    function(file, year, pollutant){
      r <- terra::rast(file)
      r %>%
        terra::crop(vect) %>%
        average_by_year() %>%
        `names<-`(paste0(names(.), "_", year, "_", pollutant)) %>%
        `varnames<-`(varnames(r)) %>%
        `longnames<-`(longnames(r)) %>%
        `units<-`(unique(units(r)))
    },
    files$file, files$year, files$pollutant
  )

  stack <- terra::rast(unname(stack))
  return(stack)
}


extract_emission <- function(vect, stack, iso2s, level=1, res="low", buffer_into_sea_km=20){


  message(glue("Extracting emissions for {iso2s}"))

  # Get geometries
  readRenviron(".Renviron")

  sector_codes <- terra::varnames(stack)
  sector_names <- terra::longnames(stack)
  unit <- unique(terra::units(stack))
  if(unit != "kg m-2 s-1") stop("Unit is not kg m-2 s-1")




  # Convert to kg/s and extract statistics
  area_r_m2 <- terra::cellSize(stack, unit="m")
  message("Converting from kg/m2/s to kg/s")
  stack_kg_s <- stack * area_r_m2

  message("Extracting zonal statistics")
  zonal_mean <- terra::extract(stack_kg_s, terra::makeValid(vect), fun="sum", ID=T, exact=T)


  emissions <- as_tibble(zonal_mean, .name_repair="minimal") %>%
    pivot_longer(cols=-c("ID"),
                 names_to="s_y_p",
                 values_to="emission") %>%
    mutate(sector_code=str_split(s_y_p, "_", simplify=T)[,1],
           year=as.integer(str_split(s_y_p, "_", simplify=T)[,2]),
           pollutant=str_split(s_y_p, "_", simplify=T)[,3]) %>%
    dplyr::select(ID, sector_code, year, pollutant, emission)

  #GID_X and NAME_X for X in 0:level
  colnames <- setdiff(c(paste0("GID_", 0:level), paste0("NAME_", 0:level)), "NAME_0")

  vect %>%
    as.data.frame() %>%
    mutate(ID=row_number()) %>%
    distinct_at(c("ID", colnames)) %>%
    left_join(emissions, by="ID",
              relationship="many-to-many") %>%
    # rename sector code -> name
    left_join(
      tibble(sector_code=sector_codes, sector=sector_names) %>%
        distinct(sector_code, sector),
      by="sector_code"
    ) %>%
    dplyr::select(-ID, -sector_code) %>%
    mutate(
      emission = emission * 365 * 24 * 3600 / 1e6,
      unit="kt/year",
    )
}


validate_emissions <- function(emissions, include_shipping=TRUE){

  years <- unique(emissions$year)
  national_emissions <- lapply(years, function(y) readRDS(glue("data/v2024_04_01/national/by_year/ceds_emissions_{y}.RDS"))) %>%
    bind_rows() %>%
    filter(include_shipping | !grepl("navigation|shipping", sector, ignore.case=T))

  # Check if the sum of provincial emissions is equal to national emissions
  emissions %>%
    filter(include_shipping | !grepl("shipping", sector, ignore.case=T)) %>%
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
        dplyr::select(iso, poll, year, emission=value) %>%
        mutate(level="national") %>%
        inner_join(
          emissions %>% distinct(poll=pollutant, iso=tolower(GID_0)),
          relationship = "many-to-many"
        ) %>%
        group_by(iso, poll, level, year) %>%
        summarise(emission=sum(emission))
    ) %>%
    ungroup() %>%
    spread(key="level", value="emission") %>%
    mutate(diff_pct=scales::percent((national-provincial)/national, accuracy=0.1))
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

download_nc <- function(year, pollutant, dir){

  dir.create(dir, showWarnings = FALSE)
  url <- glue("https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/{pollutant}/{pollutant}-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-07-08_gr_{year}01-{year}12.nc")
  dest_file <- glue("{dir}/{pollutant}_{year}.nc")
  if(file.exists(dest_file) & file.info(dest_file)$size > 1.6e8){
    return(dest_file)
  }

  # Download fails hitting timeout otherwise
  options(timeout=300)
  options(download.file.method="libcurl", url.method="libcurl")

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
