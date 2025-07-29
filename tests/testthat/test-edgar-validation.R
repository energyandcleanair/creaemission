test_that("EDGAR provincial vs national validation works correctly", {


  # Test parameters
  test_iso2 <- "ID"  # Indonesia
  test_iso3 <- "idn"  # Indonesia ISO3 code
  test_year <- 2022
  test_pollutant <- "NMVOC"
  test_sector <- "RCO"  # Gridded sector code
  known_value_kt <- 384.602  # Known value from EDGAR

  # Create source instance
  edgar_source <- EDGARSourceProvincial$new(
    version = "v8.1",
    available_years = 2022:2022
  )

  # Test 1: Get province boundaries
  vect <- edgar_source$get_province_boundaries(
    iso2 = test_iso2,
    level = 1,
    res = "low",
    buffer_into_sea_km = 20
  )

  expect_false(is.null(vect))
  expect_gt(nrow(vect), 0)
  expect_true("NAME_1" %in% names(vect))

  # Test 2: Download gridded data
  edgar_gridded <- edgar_source$download_gridded_data(
    years = test_year,
    pollutants = test_pollutant,
    sectors = test_sector
  )

  expect_equal(length(edgar_gridded$nc_files), 1)

  # Test 3: Extract provincial emissions
  edgar_emissions <- edgar_source$extract_emissions_from_grid(
    vect = vect,
    gridded_data = edgar_gridded,
    iso2 = test_iso2
  )

  expect_gt(nrow(edgar_emissions), 0)
  expect_true(all(c("iso3", "region_name", "poll", "year", "value") %in% names(edgar_emissions)))

  # Test 4: Filter for specific test case
  edgar_filtered <- edgar_emissions %>%
    filter(poll == test_pollutant,
           sector == test_sector,
           year == test_year)

  expect_gt(nrow(edgar_filtered), 0)

  # Test 5: Validate against known value (with tolerance)
  total_emissions <- sum(edgar_filtered$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.1)

  # Test 6: Check data structure
  expect_true(all(edgar_filtered$iso3 == test_iso3))
  expect_true(all(edgar_filtered$poll == test_pollutant))
  expect_true(all(edgar_filtered$year == test_year))
  expect_true(all(edgar_filtered$sector == test_sector))
})

test_that("EDGAR sector mapping works correctly", {
  # Test sector mapping utility - returns full sector names, not codes
  expect_equal(get_sector_name("RCO", "EDGAR", "provincial"), "Residential and Commercial")
  expect_equal(get_sector_name("ENE", "EDGAR", "provincial"), "Energy")

  # Test invalid sector
  expect_error(get_sector_name("INVALID_SECTOR", "EDGAR", "provincial"))
})

test_that("EDGAR national data loading works correctly", {

  test_iso3 <- "idn"
  test_sector <- "Residential and other sectors"
  test_year <- 2022
  test_pollutant <- "NMVOC"
  known_value_kt <- 384.35  # Known value from EDGAR national data

  source <- EDGARSource$new(
    version = "v8.1",
    available_years = 2022:2022
  )

  edgar_national_file <- file.path("data", "edgar", "national", paste0(test_iso3, ".rds"))

  # Skip if national data file doesn't exist
  skip_if_not(file.exists(edgar_national_file), "EDGAR national data file not found")

  edgar_national <- readRDS(edgar_national_file)

  expect_gt(nrow(edgar_national), 0)
  expect_true(all(c("iso3", "poll", "year", "sector", "value") %in% names(edgar_national)))

  # Test filtering
  filtered_data <- edgar_national %>%
    filter(poll == test_pollutant,
           year == test_year,
           sector == test_sector)

  expect_gt(nrow(filtered_data), 0)
  total_emissions <- sum(filtered_data$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.01)
  expect_true(all(filtered_data$iso3 == test_iso3))})
