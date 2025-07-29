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
