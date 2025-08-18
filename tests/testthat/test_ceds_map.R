test_that("CEDS map source works correctly", {
  # Test parameters
  test_pollutant <- "SO2"
  test_sector <- "Energy"  # This maps to sector "1" in CEDS_PROVINCIAL_SECTORS
  test_year <- 2022
  test_iso3 <- "IDN"  # Indonesia

  # Create source instance
  ceds_map <- CEDSMap$new()

  # Test 1: Clear all data and check empty
  ceds_map$clear()
  available_data <- ceds_map$list_available_data()
  expect_equal(nrow(available_data), 0)

  # Test 2: Get data when none available
  result <- ceds_map$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_null(result)

  # Test 3: Build
  ceds_map$build(pollutants = c(test_pollutant), years = test_year)

  # Test 4: Check available data after build
  available_data <- ceds_map$list_available_data()
  expect_gt(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year") %in% names(available_data)))

  # Test 5: Get data after build
  result <- ceds_map$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_false(is.null(result))
  expect_true(inherits(result, "SpatRaster"))

  # Test 6: Check raster properties
  expect_gt(terra::ncell(result), 0)
  expect_gt(terra::nlyr(result), 0)

  # Test 7: Check that raster has values
  values <- terra::values(result)
  expect_true(any(!is.na(values)))
  expect_true(any(values > 0, na.rm = TRUE))

  # Test 8: Check the unit
  expect_equal(terra::units(result), "kg m-2 yr-1")
})
