test_that("EDGAR national source sum matches raster source sum", {

  # Test parameters
  test_pollutant <- "NMVOC"
  test_sector_nationals <- c("Residential and other sectors")
  test_sector_raster <- "RCO"
  test_year <- 2022

  # Create source instances
  edgar_national <- EDGARNational$new()
  edgar_map <- EDGARMap$new()

  # Test 1: Build national data if not available
  if (nrow(edgar_national$list_available_data(pollutant = test_pollutant, year = test_year)) == 0) {
    tryCatch({
      edgar_national$build(min_year = test_year)
    }, error = function(e) {
      skip("EDGAR national data not available - build failed")
    })
  }

  # Test 2: Build map data if not available
  # edgar_map$clear()  # Clear existing data to ensure fresh build
  if (nrow(edgar_map$list_available_data(pollutant = test_pollutant, year = test_year)) == 0) {
    tryCatch({
      edgar_map$build(pollutants = c(test_pollutant), years = test_year)
    }, error = function(e) {
      skip("EDGAR map data not available - build failed")
    })
  }

  # Test 3: Get national data for all countries
  national_data <- edgar_national$get(test_pollutant, test_sector_nationals, test_year)

  if (is.null(national_data) || nrow(national_data) == 0) {
    skip("National data not available for test parameters")
  }

  # Sum all countries from national source
  national_sum_kt <- sum(national_data$value, na.rm = TRUE)

  # Test 4: Get global raster data
  global_raster <- edgar_map$get(test_pollutant, test_sector_raster, test_year, "wld")
  expect_equal(terra::units(global_raster), "kg m-2 yr-1")

  if (is.null(global_raster)) {
    stop("Global raster data not available for test parameters")
  }

  # Test 5: Calculate raster sum
  # Convert raster values to same units as national data
  # Raster is in kg/m2/s, need to convert to kt/year
  cell_areas_m2 <- terra::cellSize(global_raster, unit = "m")

  # Convert from kg/m2/s to kg/s for each cell
  raster_kg_year <- global_raster * cell_areas_m2

  # Sum all cells to get total kg/s
  raster_sum_kg_year <- sum(raster_kg_year[], na.rm = TRUE)

  # Convert to kt/year: kg/s * (365 * 24 * 3600) / (1000 * 1000)
  raster_sum_kt <- raster_sum_kg_year  / 1e6

  # Test 6: Compare the two sums
  # Allow for some tolerance due to different data sources and processing
  tolerance_pct <- 0.1  # 10% tolerance

  expect_equal(raster_sum_kt, national_sum_kt, tolerance = tolerance_pct)

  # Test 7: Log the comparison
  diff_pct <- abs(raster_sum_kt - national_sum_kt) / raster_sum_kt * 100
  message(glue::glue("Difference: {sprintf('%.2f', diff_pct)}%"))

  # Test 8: Additional validation
  expect_true(raster_sum_kt > 0, "Raster sum should be positive")
  expect_true(national_sum_kt > 0, "National sum should be positive")
})
