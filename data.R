get_emissions <- function(){
  readRDS("data/ceds_emissions_2019.RDS") %>%
    mutate(country=countrycode(iso, "iso3c", "country.name",
                               custom_match=c("GLOBAL"="Global")))
}
