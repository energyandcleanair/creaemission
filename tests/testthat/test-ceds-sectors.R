test_that("CEDS sector functions work correctly", {
  
  # Test CEDS provincial sector mapping
  expect_equal(get_sector_name("0", "CEDS", "provincial"), "Agriculture")
  expect_equal(get_sector_name("4", "CEDS", "provincial"), "Residential, Commercial, Other")
  expect_error(get_sector_name("invalid", "CEDS", "provincial"))
  
  # Test CEDS national sector mapping
  expect_equal(get_sector_name("1A4b_Residential", "CEDS", "national"), "Residential, Commercial, Other")
  expect_equal(get_sector_name("1A1a_Electricity-public", "CEDS", "national"), "Energy")
  expect_error(get_sector_name("invalid_sector", "CEDS", "national"))
  
  # Test sector validation
  expect_true(validate_sector_code("0", "CEDS", "provincial"))
  expect_true(validate_sector_code("1A4b_Residential", "CEDS", "national"))
  expect_false(validate_sector_code("invalid", "CEDS", "provincial"))
  
  # Test available sectors
  provincial_sectors <- get_available_sectors("CEDS", "provincial")
  expect_gt(length(provincial_sectors), 0)
  expect_true("0" %in% provincial_sectors)
  expect_true("4" %in% provincial_sectors)
  
  national_sectors <- get_available_sectors("CEDS", "national")
  expect_gt(length(national_sectors), 0)
  expect_true("1A4b_Residential" %in% national_sectors)
})

test_that("CEDS sector table generation works", {
  sector_table <- get_sector_table("CEDS", "provincial")
  
  expect_true(is.data.frame(sector_table))
  expect_equal(ncol(sector_table), 2)
  expect_equal(names(sector_table), c("sector_code", "sector_name"))
  expect_gt(nrow(sector_table), 0)
  
  # Check that all expected sectors are present
  expect_true("0" %in% sector_table$sector_code)
  expect_true("4" %in% sector_table$sector_code)
  expect_true("Agriculture" %in% sector_table$sector_name)
  expect_true("Residential, Commercial, Other" %in% sector_table$sector_name)
})

test_that("CEDS sector error handling works correctly", {
  # Test error for unknown source/type combination
  expect_error(get_sector_name("0", "INVALID", "provincial"))
  expect_error(get_sector_name("0", "CEDS", "invalid"))
  
  # Test error for unknown sector codes
  expect_error(get_sector_name("999", "CEDS", "provincial"))
  expect_error(get_sector_name("INVALID_SECTOR", "CEDS", "national"))
}) 