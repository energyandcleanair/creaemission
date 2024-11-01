#' Main function to collect emissions
#'
#' @param region_type country or province
#' @param iso3s
#' @param year if null, return all years available
#'
#' @return
#' @export
#'
#' @examples
get_emissions <- function(
  region_type,
  iso3s,
  years
){

  if(region_type=="country"){
    # If only one year, we make it faster by reading yearly files
    if(!is.null(years) && length(years)==1){
      get_emissions_national_by_year(iso3s=iso3s, year=years)
    }else{
      get_emissions_national(iso3s=iso3s) %>%
        filter(is.null(years) | year %in% years)
    }
  }else{
      get_emissions_provincial(iso3s=iso3s, year=years)
  }
}


get_emissions_national <- function(iso3s, years=NULL){
  lapply(tolower(iso3s), function(x){
    readRDS(glue("data/v2024_04_01/national/{x}.rds"))
  }) %>%
    bind_rows() %>%
    filter(
      is.null(years) | year %in% years
    )
}


get_emissions_national_by_year <- function(year, iso3s=NULL){
  readRDS(glue("data/v2024_04_01/national/by_year/ceds_emissions_{year}.RDS")) %>%
    mutate(
      country=countrycode(iso, "iso3c", "country.name",
                               custom_match=c("global"="Global"))
      ) %>%
    filter(is.null(iso3s) | tolower(iso) %in% tolower(iso3s))
}



get_emissions_provincial <- function(years, iso3s){

  # list files starting with any of isos
  pattern <- paste0("^(", paste(tolower(iso3s), collapse="|"), ")")
  filepaths <- list.files("data/v2024_04_01/provincial", pattern=pattern, full.names=T)
  lapply(filepaths, readRDS) %>%
    bind_rows() %>%
    mutate(fuel="All",
           unit="kt/year",
           iso=tolower(GID_0)
    ) %>%
    dplyr::select(
      poll=pollutant,
      iso,
      sector,
      fuel,
      units=unit,
      year,
      value=emission,
      country=NAME_1
    ) %>%
    filter(is.null(years) | year %in% years)
}
