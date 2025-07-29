test_that("CEDS sector functions work correctly", {
  
  # Test CEDS gridded sector mapping
  expect_equal(get_ceds_gridded_sector_name("0"), "Agriculture")
  expect_equal(get_ceds_gridded_sector_name("4"), "Residential, Commercial, Other")
  expect_equal(get_ceds_gridded_sector_name("invalid"), "invalid")
  
  # Test CEDS IPCC sector mapping
  expect_equal(get_ceds_ipcc_sector_category("1A4b_Residential"), "Residential, Commercial, Other")
  expect_equal(get_ceds_ipcc_sector_category("1A1a_Electricity-public"), "Energy")
  expect_null(get_ceds_ipcc_sector_category("invalid_sector"))
  
  # Test sector validation
  expect_true(validate_ceds_sector("0", type = "gridded"))
  expect_true(validate_ceds_sector("1A4b_Residential", type = "ipcc"))
  expect_false(validate_ceds_sector("invalid", type = "gridded"))
  
  # Test total sector detection
  expect_true(is_total_sector("TOTALS", source = "CEDS"))
  expect_true(is_total_sector("Total", source = "CEDS"))
  expect_false(is_total_sector("Agriculture", source = "CEDS"))
  
  # Test clean CEDS sector name
  expect_equal(clean_ceds_sector_name("4", preserve_code = FALSE), "Residential, Commercial, Other")
  expect_equal(clean_ceds_sector_name("4", preserve_code = TRUE), "4")
  expect_equal(clean_ceds_sector_name("1A4b_Residential", preserve_code = TRUE), "1A4b_Residential")
  expect_equal(clean_ceds_sector_name("1A4b_Residential", preserve_code = FALSE), "Residential, Commercial, Other")
  
  # Test available sectors
  gridded_sectors <- get_available_ceds_sectors(type = "gridded")
  expect_gt(length(gridded_sectors), 0)
  expect_true("0" %in% gridded_sectors)
  expect_true("4" %in% gridded_sectors)
  
  ipcc_sectors <- get_available_ceds_sectors(type = "ipcc")
  expect_gt(length(ipcc_sectors), 0)
  expect_true("1A4b_Residential" %in% ipcc_sectors)
})

test_that("CEDS sector table generation works", {
  sector_table <- get_ceds_sector_table()
  
  expect_true(is.data.frame(sector_table))
  expect_equal(ncol(sector_table), 2)
  expect_equal(names(sector_table), c("gridded_code", "gridded_name"))
  expect_gt(nrow(sector_table), 0)
  
  # Check that all expected sectors are present
  expect_true("0" %in% sector_table$gridded_code)
  expect_true("4" %in% sector_table$gridded_code)
  expect_true("Agriculture" %in% sector_table$gridded_name)
  expect_true("Residential, Commercial, Other" %in% sector_table$gridded_name)
})

test_that("CEDS sector filtering works correctly", {
  # Create test data
  test_data <- data.frame(
    sector = c("0", "4", "TOTALS", "Agriculture", "Residential, Commercial, Other"),
    value = c(100, 200, 500, 150, 250)
  )
  
  # Test filtering out total sectors
  filtered_data <- filter_out_total_sectors(test_data, source = "CEDS")
  expect_equal(nrow(filtered_data), 4)  # Should remove "TOTALS"
  expect_false("TOTALS" %in% filtered_data$sector)
  
  # Test getting visualization sectors
  viz_sectors <- get_visualization_sectors(test_data, source = "CEDS")
  expect_equal(length(viz_sectors), 4)
  expect_false("TOTALS" %in% viz_sectors)
  
  # Test generic filtering
  generic_filtered <- filter_sectors_for_viz(test_data, source = "CEDS")
  expect_equal(nrow(generic_filtered), 4)
  
  # Test with totals included
  with_totals <- filter_sectors_for_viz(test_data, source = "CEDS", include_totals = TRUE)
  expect_equal(nrow(with_totals), 5)
}) 