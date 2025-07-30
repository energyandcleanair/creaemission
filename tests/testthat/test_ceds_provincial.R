test_that("CEDS provincial source works correctly", {

  test_iso3 <- "IDN"
  test_sector <- "Residential, Commercial, Other"  # This is what sector "4" maps to
  test_year <- 2022
  test_pollutant <- "NMVOC"
  known_value_kt <- 404.34  # Known value from CEDS

  # Create source instance
  ceds_provincial <- CEDSProvincial$new()

  # Test 1: Clear all data and check empty
  ceds_provincial$clear()
  available_data <- ceds_provincial$list_available_data()
  expect_equal(nrow(available_data), 0)

  # Test 2: Get data when none available
  result <- ceds_provincial$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_null(result)

  # Test 3: Build data with no buffering to match known value
  ceds_provincial$build(
    iso2s = c("ID"),  # Keep iso2s for build method as it expects iso2
    years = test_year,
    pollutants = c(test_pollutant),
    buffer_into_sea_km = 0  # No buffering for test to match known value
  )

  # Test 4: Check available data after build
  available_data <- ceds_provincial$list_available_data()
  expect_gt(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year", "iso3") %in% names(available_data)))

  # Test 5: Get data after build
  result <- ceds_provincial$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_false(is.null(result))
  expect_gt(nrow(result), 0)
  expect_true(all(c("iso3", "poll", "sector", "year", "value", "region_name") %in% names(result)))

  # Test 6: Validate against known value
  total_emissions <- sum(result$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.1)

  # Test 7: Check data structure
  expect_true(all(result$poll == test_pollutant))
  expect_true(all(result$sector == test_sector))
  expect_true(all(result$year == test_year))
  expect_true(all(result$iso3 == "idn"))

  # Clear because we used 0km instead of default value
  ceds_provincial$clear()
})
