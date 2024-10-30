
get_countries_with_provincial_data <- function(){
  list.files("data/v2024_04_01/by_province", pattern=".*\\.rds") %>%
    # IN_2020.rds -> IN
    map_chr(~str_extract(.x, "^[A-Z]{2}")) %>%
    unique()
}


get_national_emissions_years <- function(){
  list.files("data/v2024_04_01", pattern=".*\\.RDS") %>%
    # ceds_emissions_2000.RDS -> 2000
    map_chr(~str_extract(.x, "\\d{4}")) %>%
    as.integer()%>%
    unique()
}


get_provoncial_emissions_years <- function(){
  list.files("data/v2024_04_01/by_province", pattern=".*\\.rds") %>%
    # ceds_emissions_2000.RDS -> 2000
    map_chr(~str_extract(.x, "\\d{4}")) %>%
    as.integer() %>%
    unique()
}

get_emissions_raster <- function(poll, year){
  raster::stack(glue('data/v2024_04_01/old/{poll}_emissions_{year}.tif'))
}
