get_countries <- function(source = "CEDS"){
  get_emissions_national_by_year(year=2022, source=source) %>%
    distinct(iso) %>%
    mutate(country = countrycode(iso, "iso3c", "country.name",
                                custom_match=c("global"="Global")))
}

get_countries_with_provincial_data <- function(source = "CEDS"){
  source_dir <- tolower(source)
  list.files(glue("data/{source_dir}/provincial"), pattern=".*\\.rds") %>%
    # IN_2020.rds -> IN
    map_chr(~str_extract(.x, "^[A-Z|a-z]{3}")) %>%
    unique()
}

get_national_emissions_years <- function(source = "CEDS"){
  source_dir <- tolower(source)
  source_prefix <- tolower(source)
  
  list.files(glue("data/{source_dir}/national/by_year"), pattern=".*\\.rds") %>%
    # ceds_emissions_2000.RDS -> 2000
    map_chr(~str_extract(.x, "\\d{4}")) %>%
    as.integer() %>%
    unique()
}

get_provincial_emissions_years <- function(source = "CEDS"){
  if(toupper(source) == "CEDS") {
    seq(2000, 2022)
  } else if(toupper(source) == "EDGAR") {
    seq(2000, 2022)
  }
}

get_emissions_raster <- function(poll, year, source = "CEDS"){
  source_dir <- tolower(source)
  source_prefix <- tolower(source)
  
  raster::stack(glue('data/{source_dir}/raster/{poll}_emissions_{year}.tif'))
}
