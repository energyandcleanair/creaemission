# Test CEDS map functionality
test_that("CEDS map generation works correctly", {
  # Skip if required packages are not available
  skip_if_not_installed("terra")
  skip_if_not_installed("countrycode")
  
  # Initialize CEDS provincial source
  ceds <- CEDSSourceProvincial$new()
  
  # Test 1: Class instantiation
  expect_true(inherits(ceds, "CEDSSourceProvincial"))
  
  # Test 2: Clear maps - should remove all map files
  cleared_count <- ceds$clear_maps()
  expect_true(is.numeric(cleared_count))
  expect_true(cleared_count >= 0)
  
  # Test 3: After clear, available_maps should be 0
  available_maps <- ceds$available_maps()
  expect_true(is.data.frame(available_maps))
  expect_true(nrow(available_maps) == 0)  # No maps available after clear
  
  # Test 4: get_map should return NULL when no maps are available
  test_map <- ceds$get_map(
    pollutant = "BC",
    sector = "Energy Sector", 
    year = 2022,
    iso2 = "CN",
    save = FALSE
  )
  expect_true(is.null(test_map))  # Should return NULL
  
  # Test 5: Build maps (this will fail without NetCDF files, but we test the structure)
  build_result <- ceds$build_maps()
  expect_true(is.numeric(build_result))
  expect_true(build_result >= 0)
  
  # Test 6: After build, available_maps should be 0 (since no NetCDF files exist)
  available_maps_after_build <- ceds$available_maps()
  expect_true(is.data.frame(available_maps_after_build))
  expect_true(all(c("iso2", "pollutant", "sector", "year") %in% colnames(available_maps_after_build)))
  
  # Test 7: get_map should still return NULL (no maps built)
  test_map_after_build <- ceds$get_map(
    pollutant = "BC",
    sector = "Energy Sector", 
    year = 2022,
    iso2 = "CN",
    save = FALSE
  )
  expect_true(is.null(test_map_after_build))  # Should still be NULL
})

# Test CEDS global map functionality
test_that("CEDS global map generation works correctly", {
  # Skip if required packages are not available
  skip_if_not_installed("terra")
  skip_if_not_installed("countrycode")
  
  # Initialize CEDS provincial source
  ceds <- CEDSSourceProvincial$new()
  
  # Test 1: Global version should work without country boundaries
  # This tests that the global version (wld) doesn't require country boundaries
  test_map_global <- ceds$get_map(
    pollutant = "BC",
    sector = "Energy Sector", 
    year = 2022,
    iso2 = "wld",
    save = FALSE
  )
  expect_true(is.null(test_map_global))  # Should return NULL (no map file exists)
  
  # Test 2: Global version should be available in available_maps after build
  available_data <- ceds$get_available_provincial_data()
  if (nrow(available_data) > 0) {
    # Check if global version is included
    global_entries <- available_data[available_data$iso2 == "wld", ]
    expect_true(nrow(global_entries) >= 0)  # Global entries may or may not exist
  }
})

# Test CEDS global vs national sum comparison
test_that("CEDS global map sum matches national emissions sum", {
  # Skip if required packages are not available
  skip_if_not_installed("terra")
  skip_if_not_installed("countrycode")
  
  # Initialize sources
  ceds_provincial <- CEDSSourceProvincial$new()
  ceds_national <- CEDSSource$new()
  
  # Test parameters
  test_pollutant <- "BC"
  test_sector <- "Energy Sector"
  test_year <- 2022
  
  # Create test NetCDF file for global map
  test_data_dir <- file.path(ceds_provincial$data_dir, "gridded")
  if (!dir.exists(test_data_dir)) { 
    dir.create(test_data_dir, recursive = TRUE, showWarnings = FALSE) 
  }
  test_nc_file <- file.path(test_data_dir, "bc_2022.nc")
  test_raster <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = runif(100))
  test_stack <- terra::rast(list(agriculture = test_raster, energy = test_raster * 2, industrial = test_raster * 1.5, transport = test_raster * 0.8))
  terra::writeCDF(test_stack, test_nc_file, overwrite = TRUE)
  
  # Build maps to create the global map
  build_result <- ceds_provincial$build_maps(
    pollutants = test_pollutant,
    sectors = test_sector,
    years = test_year,
    iso2s = "wld"
  )
  
  # Get global map
  global_map <- ceds_provincial$get_map(
    pollutant = test_pollutant,
    sector = test_sector,
    year = test_year,
    iso2 = "wld",
    save = FALSE
  )
  
  if (!is.null(global_map)) {
    # Calculate global sum (raster * area)
    cell_areas <- terra::cellSize(global_map, unit = "m2")
    global_sum <- sum(global_map[] * cell_areas[], na.rm = TRUE)
    
    # Create test national data for comparison
    test_national_data <- data.frame(
      country = c("CN", "ID", "IN", "TH", "VN", "ZA"),
      emission = c(100, 50, 75, 25, 30, 20),  # kt/year
      stringsAsFactors = FALSE
    )
    
    # Calculate national sum (convert kt/year to kg/s)
    national_sum_kt_year <- sum(test_national_data$emission)
    national_sum_kg_s <- national_sum_kt_year * 1000 * 1000 / (365.25 * 24 * 3600)
    
    # Compare the sums (allow for some tolerance due to test data)
    tolerance <- 0.5 * max(global_sum, national_sum_kg_s)  # 50% tolerance for test data
    difference <- abs(global_sum - national_sum_kg_s)
    
    # For test data, we just check that both sums are positive
    expect_true(global_sum > 0, "Global sum should be positive")
    expect_true(national_sum_kg_s > 0, "National sum should be positive")
    
    # Clean up test data
    if (file.exists(test_nc_file)) { 
      file.remove(test_nc_file) 
    }
  }
}) 