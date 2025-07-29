test_that("CEDS provincial vs national validation works correctly", {

  # Test parameters
  test_iso2 <- "ID"  # Indonesia
  test_iso3 <- "idn"  # Indonesia ISO3 code
  test_year <- 2022
  test_pollutant <- "NMVOC"
  test_sector <- "4"  # CEDS sector code (numeric format from netCDF)
  known_value_kt <- 404.34  # Known value from CEDS

  # Create source instance
  ceds_source <- CEDSSourceProvincial$new()

  # Test 1: Get province boundaries
  province_boundaries <- ceds_source$get_province_boundaries(
    test_iso2,
    buffer_into_sea_km = 10
  )

  expect_false(is.null(province_boundaries))
  expect_gt(nrow(province_boundaries), 0)

  # Test 2: Download CEDS gridded data
  gridded_data <- ceds_source$download_gridded_data(
    years = test_year,
    pollutants = test_pollutant
  )

  expect_gt(length(gridded_data$nc_files), 0)

  # Test 3: Extract CEDS provincial emissions
  # Use preserve_sector_codes=TRUE for validation to keep original sector codes
  # debug(ceds_source$extract_emissions_from_grid)
  ceds_provincial <- ceds_source$extract_emissions_from_grid(
    vect = province_boundaries,
    gridded_data = gridded_data,
    iso2 = test_iso2,
    preserve_sector_codes = TRUE
  )

  expect_gt(nrow(ceds_provincial), 0)
  expect_true(all(c("iso3", "region_name", "poll", "year", "value") %in% names(ceds_provincial)))

  # Test 4: Filter for test sector
  # With preserve_sector_codes=TRUE, we should get the original sector code
  ceds_filtered <- ceds_provincial %>%
    filter(sector == test_sector,
           poll == test_pollutant,
           year == test_year)

  expect_gt(nrow(ceds_filtered), 0)

  # Test 5: Validate against known value (with higher tolerance i.e. 10%)
  total_emissions <- sum(ceds_filtered$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.1)

  # Test 6: Check data structure
  expect_true(all(ceds_filtered$iso3 == test_iso3))
  expect_true(all(ceds_filtered$poll == test_pollutant))
  expect_true(all(ceds_filtered$year == test_year))
  expect_true(all(ceds_filtered$sector == test_sector))
})

test_that("CEDS national data loading works correctly", {

  test_iso3 <- "idn"
  test_sector <- "1A4b_Residential"
  test_year <- 2022
  test_pollutant <- "NMVOC"
  known_value_kt <- 404.34  # Known value from CEDS

  ceds_national_file <- file.path("data", "ceds", "national", paste0(test_iso3, ".rds"))

  # Skip if national data file doesn't exist
  skip_if_not(file.exists(ceds_national_file), "CEDS national data file not found")

  ceds_national <- readRDS(ceds_national_file)

  expect_gt(nrow(ceds_national), 0)
  expect_true(all(c("iso3", "poll", "year", "sector", "value") %in% names(ceds_national)))

  # Test filtering
  filtered_data <- ceds_national %>%
    filter(poll == test_pollutant,
           year == test_year,
           sector == test_sector)

  expect_gt(nrow(filtered_data), 0)
  total_emissions <- sum(filtered_data$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.01)
})
