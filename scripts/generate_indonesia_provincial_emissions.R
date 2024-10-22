emissions_id_low <- lapply(c(2019,2022), function(y){
  extract_provincial_data(
    year=y,
    country_id="ID",
    level=1,
    res="low"
  )
}) %>%
  bind_rows()


validate_emissions(emissions_id_low, T)
validate_emissions(emissions_id_low, F)


emissions_id_low %>%
  select(GID_0, GID_1, NAME_1, year, sector=sector_code, emission, pollutant) %>%
  mutate(unit="kt/year") %>%
  # split by year and writee to yearly files
  split(.$year) %>%
  imap(~write_csv(.x, glue("output/emissions_id_{.y}.csv")))

