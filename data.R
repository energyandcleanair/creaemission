get_emissions <- function(){
  readRDS("data/ceds_emissions_2019.RDS") %>%
    mutate(country=countrycode(iso, "iso3c", "country.name",
                               custom_match=c("GLOBAL"="Global")))
}

get_emissions_cities <- function(){
  readRDS('data/ceds_emissions_2019_cities.RDS')
}

get_emissions_raster <- function(poll){
  raster::stack('data/nox_emissions_2019.tif')
}
