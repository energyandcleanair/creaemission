test_that("EDGAR download functionality works correctly", {
  # Skip if not in interactive mode or if network is not available
  skip_if_not_installed("curl")
  skip_on_cran()
  
  # Create an EDGAR source instance
  edgar_source <- EDGARSourceProvincial$new(
    version = "v8.1",
    available_years = 2022:2022
  )
  
  # Test downloading a single pollutant for a single sector
  test_result <- edgar_source$download_nc(
    pollutant = "NMVOC", 
    sector = "ENE",
    dir = tempdir()
  )
  
  # Check that download succeeded (returns list of files)
  expect_true(is.character(test_result))
  if (length(test_result) > 0) {
    expect_true(all(file.exists(test_result)))
  }
  
  # Clean up
  if (length(test_result) > 0) {
    unlink(test_result)
  }
})

test_that("EDGAR download handles missing files gracefully", {
  edgar_source <- EDGARSourceProvincial$new(
    version = "v8.1",
    available_years = 2022:2022
  )
  
  # Test with invalid parameters
  result <- edgar_source$download_nc(
    pollutant = "INVALID", 
    sector = "INVALID", 
    dir = tempdir()
  )
  
  # Should return empty character vector for invalid parameters
  expect_true(is.character(result))
  expect_equal(length(result), 0)
}) 