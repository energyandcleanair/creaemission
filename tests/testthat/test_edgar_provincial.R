test_that("EDGAR provincial source works correctly", {
  # Test parameters
  test_iso3 <- "IDN"  # Indonesia
  test_pollutant <- "NMVOC"
  test_sector <- "RCO"  # Gridded sector code
  test_year <- 2022
  known_value_kt <- 384.602  # Known value from EDGAR for IND NMVOC emissions in 2022

  # Create source instance with test data directory
  test_data_dir <- get_test_data_path("edgar", "provincial")
  edgar_provincial <- EDGARProvincial$new(data_dir = test_data_dir)

  # Test 1: Check empty test data directory
  available_data <- edgar_provincial$list_available_data()
  expect_equal(nrow(available_data), 0)

  # Test 2: Get data when none available
  result <- edgar_provincial$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_null(result)

  # Test 3: Build data (this will use cache for downloads, but save to test data dir)
  edgar_provincial$build(
    iso2s = c("ID"),  # Keep iso2s for build method as it expects iso2
    years = test_year,
    pollutants = c(test_pollutant),
    sectors = c(test_sector, "ENE", "SWD_LDF") # Add one sector to make sure it works with several
  )

  # Test 4: Check available data after build
  available_data <- edgar_provincial$list_available_data()
  expect_gt(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year", "iso3") %in% names(available_data)))

  # Test 5: Get data after build
  result <- edgar_provincial$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_false(is.null(result))
  expect_gt(nrow(result), 0)
  expect_true(all(c("iso3", "poll", "sector", "year", "value", "region_name") %in% names(result)))

  # Test 6: Validate against known value
  total_emissions <- sum(result$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.1)

  # Test 7: Check data structure
  expect_true(all(result$poll == test_pollutant))
  expect_true(all(result$sector == map_values(test_sector, EDGAR_PROVINCIAL_SECTOR_MAPPING)))
  expect_true(all(result$year == test_year))
  expect_true(all(result$iso3 == "idn"))

  # Test 8: Verify data is saved to test directory, not main data
  test_files <- list.files(test_data_dir, recursive = TRUE)
  expect_gt(length(test_files), 0)

  # Test 9: Verify class inheritance
  expect_true(inherits(edgar_provincial, "EDGARProvincial"))
  expect_true(inherits(edgar_provincial, "SourceProvincial"))

  # Clean up test data
  edgar_provincial$clear()
  available_data <- edgar_provincial$list_available_data()
  expect_equal(nrow(available_data), 0)
})
