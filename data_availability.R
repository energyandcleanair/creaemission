get_countries <- function(){
  get_emissions_national_by_year(year=2022) %>%
    distinct(iso) %>%
    mutate(country = countrycode(iso, "iso3c", "country.name",
                                custom_match=c("global"="Global")))
}


get_countries_with_provincial_data <- function(){
  list.files("data/v2024_04_01/provincial", pattern=".*\\.rds") %>%
    # IN_2020.rds -> IN
    map_chr(~str_extract(.x, "^[A-Z|a-z]{3}")) %>%
    unique()
}


get_national_emissions_years <- function(){
  list.files("data/v2024_04_01/national/by_year", pattern=".*\\.RDS") %>%
    # ceds_emissions_2000.RDS -> 2000
    map_chr(~str_extract(.x, "\\d{4}")) %>%
    as.integer()%>%
    unique()
}


get_provoncial_emissions_years <- function(){
  seq(2000, 2022)
}


get_emissions_raster <- function(poll, year){
  raster::stack(glue('data/v2024_04_01/old/{poll}_emissions_{year}.tif'))
}
