get_emissions_by_year <- function(year){
  readRDS(glue("data/v2024_04_01/ceds_emissions_{year}.RDS")) %>%
    mutate(country=countrycode(iso, "iso3c", "country.name",
                               custom_match=c("global"="Global")))
}

get_emissions_by_countries <- function(isos){
  lapply(isos, function(x){
    readRDS(glue("data/v2024_04_01/by_country/{x}.rds"))
  }) %>%
    bind_rows()
}

get_emissions_years <- function(){
  list.files("data/v2024_04_01", pattern=".*\\.RDS") %>%
    # ceds_emissions_2000.RDS -> 2000
    map_chr(~str_extract(.x, "\\d{4}")) %>%
    as.integer()
}

# get_emissions_cities <- function(){
#   readRDS('data/ceds_emissions_2019_cities.RDS')
# }

get_emissions_raster <- function(poll, year){
  raster::stack(glue('data/v2024_04_01/old/{poll}_emissions_{year}.tif'))
}

