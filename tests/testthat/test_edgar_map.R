test_that("EDGAR map source works correctly", {
  # Test parameters
  test_pollutant <- "NMVOC"
  test_sector <- "RCO"
  test_year <- 2022
  test_iso3 <- "IDN"  # Indonesia

  # Create source instance
  edgar_map <- EDGARMap$new()

  # Test 1: Clear all data and check empty
  edgar_map$clear()
  available_data <- edgar_map$list_available_data()
  expect_equal(nrow(available_data), 0)

  # Test 2: Get data when none available
  result <- edgar_map$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_null(result)

  # Test 3: Build downloads gridded NetCDF; default keep_raw_cache clears cache/edgar/gridded after use
  edgar_map$build(pollutants = c(test_pollutant), years = test_year)

  # Shared gridded cache may contain other pollutants from other tests; only assert this build's raw files are gone
  gridded_dir <- file.path(edgar_map$cache_dir, "gridded")
  if (dir.exists(gridded_dir)) {
    pat <- paste0("^", test_pollutant, "_")
    leftover <- list.files(gridded_dir, pattern = pat)
    expect_equal(length(leftover), 0)
  }

  # Test 4: Check available data after build (skip for now)
  available_data <- edgar_map$list_available_data()
  expect_gt(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year") %in% names(available_data)))

  # Test 5: Get data after build (skip for now)
  result <- edgar_map$get(test_pollutant, test_sector, test_year, test_iso3)
  expect_false(is.null(result))
  expect_true(inherits(result, "SpatRaster"))

  # Test 6: Check raster properties (skip for now)
  expect_gt(terra::ncell(result), 0)
  expect_gt(terra::nlyr(result), 0)

  # Test 7: Check that raster has values (skip for now)
  values <- terra::values(result)
  expect_true(any(!is.na(values)))
  expect_true(any(values > 0, na.rm = TRUE))

  # Test 8: Check that raster has correct units (skip for now)
  expect_equal(terra::units(result), "kg m-2 yr-1")

  # For now, just test that the class can be instantiated and basic methods work
  expect_true(inherits(edgar_map, "EDGARMap"))
  expect_true(inherits(edgar_map, "SourceMap"))


  # Test that list_available_data returns empty when no data
  edgar_map$clear()
  available_data <- edgar_map$list_available_data()
  expect_equal(nrow(available_data), 0)
})
