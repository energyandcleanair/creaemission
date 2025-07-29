#' Main function to collect emissions
#'
#' @param region_type country or province
#' @param iso3s ISO3 country codes
#' @param years Years to retrieve data for
#' @param source Data source (CEDS or EDGAR)
#'
#' @return Emissions data frame
#' @export
#'
#' @examples
get_emissions <- function(
  region_type,
  iso3s,
  years,
  source = SOURCE_CEDS,
  catalog = get_emissions_catalog()
){
  source <- toupper(source)

  # Validate inputs against catalog
  if (!is.null(years)) {
    available_years <- get_catalog_years(catalog=catalog, source=source, type=region_type)
    if (!all(years %in% available_years)) {
      warning(glue::glue("Some requested years not available for {source} {region_type} data"))
    }
  }

  if (!is.null(iso3s)) {
    available_countries <- get_catalog_countries(catalog=catalog, source=source, type=region_type)
    if (!all(tolower(iso3s) %in% tolower(available_countries))) {
      warning(glue::glue("Some requested countries not available for {source} {region_type} data"))
    }
  }

  if(region_type == REGIONTYPE_NATIONAL){
    # If only one year, we make it faster by reading yearly files
    if(!is.null(years) && length(years)==1){
      get_emissions_national_by_year(year=years, iso3s=iso3s, source=source, catalog=catalog)
    }else{
      get_emissions_national(iso3s=iso3s, years=years, source=source, catalog=catalog)
    }
  }else{
      get_emissions_provincial(years=years, iso3s=iso3s, source=source, catalog=catalog)
  }
}


get_emissions_national <- function(iso3s, years=NULL, source=SOURCE_CEDS, catalog = get_emissions_catalog()) {
  filepaths <- catalog %>%
    dplyr::filter(
      source == toupper(source),
      type == REGIONTYPE_NATIONAL,
      iso3 %in% tolower(iso3s)
    ) %>%
    pull(filepath)

  lapply(filepaths, readRDS) %>%
    bind_rows() %>%
    filter(is.null(years) | year %in% years) %>%
    mutate(
      country = iso3_to_country(iso3),
      source = source)

}


get_emissions_national_by_year <- function(year, iso3s=NULL, source="CEDS", catalog = get_emissions_catalog()) {

  filepath <- catalog %>%
    dplyr::filter(
      tolower(source) == tolower(!!source),
      type == "national"
    ) %>%
    pull(yearly_filepath) %>%
    .[[1]] %>%  # get the list of yearly files
    grep(pattern = paste0(".*", year, "\\.rds$"), value = TRUE)

  readRDS(filepath) %>%
    mutate(
      country = iso3_to_country(iso3),
      source = source
    ) %>%
    filter(is.null(iso3s) | tolower(iso3) %in% tolower(iso3s))
}



get_emissions_provincial <- function(years, iso3s, source="CEDS", catalog = get_emissions_catalog()) {
  filepaths <- catalog %>%
    dplyr::filter(
      tolower(source) == tolower(!!source),
      type == "provincial",
      iso3 %in% tolower(iso3s)
    ) %>%
    pull(filepath)

  lapply(filepaths, readRDS) %>%
    bind_rows() %>%
    mutate(
      fuel = "All",
      unit = "kt/year",
      iso3 = tolower(GID_0),
      source = source
    ) %>%
    dplyr::select(
      poll = pollutant,
      iso3,
      sector,
      fuel,
      units = unit,
      year,
      value = emission,
      country = NAME_1,
      source
    ) %>%
    filter(is.null(years) | year %in% years)
}

#' Get the latest available year for a given source and region type
#'
#' @param source Data source (CEDS or EDGAR)
#' @param region_type country or province
#' @param catalog Emissions catalog
#' @return Latest available year
#' @export
get_latest_year <- function(source = SOURCE_CEDS, region_type = REGIONTYPE_NATIONAL, catalog = get_emissions_catalog()) {
  available_years <- get_catalog_years(catalog = catalog, source = toupper(source), type = region_type)
  max(available_years)
}

#' Get emissions raster for mapping
#'
#' @param poll Pollutant code
#' @param year Year
#' @param sector Sector code
#' @param source Data source (CEDS or EDGAR)
#' @param iso2 ISO2 country code (for provincial data)
#' @return Terra raster object
#' @export
get_emissions_raster <- function(poll, year, sector, source = "CEDS", iso2 = NULL) {
  source <- toupper(source)
  
  # Create appropriate source object
  if (source == "CEDS") {
    source_obj <- CEDSSourceProvincial$new()
  } else if (source == "EDGAR") {
    source_obj <- EDGARSourceProvincial$new()
  } else {
    stop(glue::glue("Unsupported source: {source}"))
  }
  
  # If no specific country requested, try to find one with data
  if (is.null(iso2)) {
    available_maps <- source_obj$available_maps()
    if (nrow(available_maps) == 0) {
      stop(glue::glue("No map data available for {source}"))
    }
    
    # Find a country with the requested parameters
    matching_data <- available_maps %>%
      dplyr::filter(
        pollutant == poll,
        sector == sector,
        year == year
      )
    
    if (nrow(matching_data) == 0) {
      stop(glue::glue("No data found for {poll} {sector} {year} in {source}"))
    }
    
    # Use the first available country
    iso2 <- matching_data$iso2[1]
  }
  
  # Get the map raster
  raster_obj <- source_obj$get_map(
    pollutant = poll,
    sector = sector,
    year = year,
    iso2 = iso2,
    save = FALSE
  )
  
  if (is.null(raster_obj)) {
    stop(glue::glue("Failed to generate raster for {poll} {sector} {year} {iso2} in {source}"))
  }
  
  return(raster_obj)
}

#' @title Compare Global Map Sum with National Emissions Sum
#' @description Compare the sum of a global emissions map (raster * area) with the sum of all national emissions for the same pollutant/sector/year
#' @param pollutant Pollutant code
#' @param sector Sector code  
#' @param year Year
#' @param source Data source ("CEDS" or "EDGAR")
#' @return List with comparison results
#' @export
compare_global_vs_national <- function(pollutant, sector, year, source = "CEDS") {
  
  # Initialize sources
  if (source == "CEDS") {
    provincial_source <- CEDSSourceProvincial$new()
    national_source <- CEDSSource$new()
  } else if (source == "EDGAR") {
    provincial_source <- EDGARSourceProvincial$new()
    national_source <- EDGARSource$new()
  } else {
    stop("Source must be 'CEDS' or 'EDGAR'")
  }
  
  # Get global map
  global_map <- provincial_source$get_map(
    pollutant = pollutant,
    sector = sector,
    year = year,
    iso2 = "wld",
    save = FALSE
  )
  
  if (is.null(global_map)) {
    return(list(
      success = FALSE,
      error = "Global map not available"
    ))
  }
  
  # Calculate global sum (raster * area)
  cell_areas <- terra::cellSize(global_map, unit = "m2")
  global_sum_kg_s <- sum(global_map[] * cell_areas[], na.rm = TRUE)
  
  # Get national emissions
  national_emissions <- national_source$get_emissions(
    pollutant = pollutant,
    sector = sector,
    year = year
  )
  
  if (is.null(national_emissions) || nrow(national_emissions) == 0) {
    return(list(
      success = FALSE,
      error = "National emissions not available"
    ))
  }
  
  # Sum all national emissions
  national_sum_kt_year <- sum(national_emissions$emission, na.rm = TRUE)
  
  # Convert national sum to same units as global map (kt/year to kg/s)
  national_sum_kg_s <- national_sum_kt_year * 1000 * 1000 / (365.25 * 24 * 3600)
  
  # Calculate difference and percentage
  difference <- abs(global_sum_kg_s - national_sum_kg_s)
  percentage_diff <- (difference / max(global_sum_kg_s, national_sum_kg_s)) * 100
  
  return(list(
    success = TRUE,
    pollutant = pollutant,
    sector = sector,
    year = year,
    source = source,
    global_sum_kg_s = global_sum_kg_s,
    national_sum_kt_year = national_sum_kt_year,
    national_sum_kg_s = national_sum_kg_s,
    difference_kg_s = difference,
    percentage_difference = percentage_diff,
    tolerance_met = percentage_diff <= 10  # 10% tolerance
  ))
}
