test_that("EDGAR national source works correctly", {

  # Test parameters
  test_pollutant <- "NMVOC"
  test_sector <- "Residential and other sectors"
  test_year <- 2022
  test_iso3 <- "IDN"
  known_value_kt <- 384.35  # Known value from EDGAR national data

  # Create source instance
  edgar_source <- EDGARNational$new()

  # Test 1: Clear all data and check empty
  edgar_source$clear()
  available_data <- edgar_source$list_available_data()
  expect_equal(nrow(available_data), 0)

  # Test 2: Get data when none available
  result <- edgar_source$get(test_pollutant, test_sector, test_year)
  expect_null(result)

  # Test 3: Build data
  edgar_source$build(min_year = 2022)

  # Test 4: Check available data after build
  available_data <- edgar_source$list_available_data()
  expect_gt(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year") %in% names(available_data)))

  # Test 5: Get data after build
  result <- edgar_source$get(test_pollutant, test_sector, test_year, iso
  expect_false(is.null(result))
  expect_gt(nrow(result), 0)
  expect_true(all(c("iso3", "poll", "sector", "year", "value") %in% names(result)))

  # Test 6: Validate against known value
  total_emissions <- sum(result$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.01)

  # Test 7: Check data structure
  expect_true(all(result$poll == test_pollutant))
  expect_true(all(result$sector == test_sector))
  expect_true(all(result$year == test_year))
})
