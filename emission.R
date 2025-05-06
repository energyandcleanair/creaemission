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
  source = "CEDS"
){
  source <- toupper(source)
  
  if(region_type=="country"){
    # If only one year, we make it faster by reading yearly files
    if(!is.null(years) && length(years)==1){
      get_emissions_national_by_year(iso3s=iso3s, year=years, source=source)
    }else{
      get_emissions_national(iso3s=iso3s, source=source) %>%
        filter(is.null(years) | year %in% years)
    }
  }else{
      get_emissions_provincial(iso3s=iso3s, year=years, source=source)
  }
}


get_emissions_national <- function(iso3s, years=NULL, source="CEDS"){
  source_dir <- tolower(source)
  
  lapply(tolower(iso3s), function(x){
    readRDS(glue("data/{source_dir}/national/{x}.rds"))
  }) %>%
    bind_rows() %>%
    filter(
      is.null(years) | year %in% years
    ) %>%
    mutate(source = source)
}


get_emissions_national_by_year <- function(year, iso3s=NULL, source="CEDS"){
  source_dir <- tolower(source)
  source_prefix <- tolower(source)
  
  readRDS(glue("data/{source_dir}/national/by_year/{source_prefix}_emissions_{year}.rds")) %>%
    mutate(
      country=countrycode(iso, "iso3c", "country.name",
                               custom_match=c("global"="Global")),
      source = source
    ) %>%
    filter(is.null(iso3s) | tolower(iso) %in% tolower(iso3s))
}



get_emissions_provincial <- function(years, iso3s, source="CEDS"){
  source_dir <- tolower(source)
  
  # list files starting with any of isos
  pattern <- paste0("^(", paste(tolower(iso3s), collapse="|"), ")")
  filepaths <- list.files(glue("data/{source_dir}/provincial"), pattern=pattern, full.names=T)
  lapply(filepaths, readRDS) %>%
    bind_rows() %>%
    mutate(fuel="All",
           unit="kt/year",
           iso=tolower(GID_0),
           source = source
    ) %>%
    dplyr::select(
      poll=pollutant,
      iso,
      sector,
      fuel,
      units=unit,
      year,
      value=emission,
      country=NAME_1,
      source
    ) %>%
    filter(is.null(years) | year %in% years)
}
