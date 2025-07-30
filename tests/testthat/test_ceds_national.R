test_that("CEDS national source works correctly", {
  # Test parameters
  test_iso3 <- "idn"
  test_sector <- "1A4b_Residential"
  test_year <- 2022
  test_pollutant <- "NMVOC"
  known_value_kt <- 404.34  # Known value from CEDS


  # Create source instance
  ceds_source <- CEDSNational$new()

  # Test 1: Clear all data and check empty
  ceds_source$clear()
  available_data <- ceds_source$list_available_data()
  expect_equal(nrow(available_data), 0)

  # Test 2: Get data when none available
  result <- ceds_source$get(test_pollutant, test_sector, test_year)
  expect_null(result)

  # Test 3: Build data
  ceds_source$build(min_year = 2022)

  # Test 4: Check available data after build
  available_data <- ceds_source$list_available_data()
  expect_gt(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year") %in% names(available_data)))

  # Test 5: Get data after build
  result <- ceds_source$get(test_pollutant, test_sector, test_year) %>%
    filter(iso3== test_iso3)
  expect_false(is.null(result))
  expect_gt(nrow(result), 0)
  expect_true(all(c("iso3", "poll", "sector", "year", "value") %in% names(result)))

  # Test 6: Validate against known value
  total_emissions <- sum(result$value, na.rm = TRUE)
  expect_equal(total_emissions, known_value_kt, tolerance = 0.1)

  # Test 7: Check data structure
  expect_true(all(result$poll == test_pollutant))
  expect_true(all(result$sector == test_sector))
  expect_true(all(result$year == test_year))
})
