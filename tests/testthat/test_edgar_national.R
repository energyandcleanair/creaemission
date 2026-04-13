test_that("EDGAR national constructor uses isolated paths offline", {
  paths <- new_test_source_paths("edgar")

  edgar_source <- EDGARNational$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir
  )

  expect_false(edgar_source$use_prebuilt_available_data)
  expect_equal(normalizePath(edgar_source$data_dir), normalizePath(paths$data_dir))
  expect_equal(normalizePath(edgar_source$cache_dir), normalizePath(paths$cache_dir))

  expect_equal(edgar_source$clear(), 0)

  available_data <- edgar_source$list_available_data()
  expect_equal(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year", "iso3") %in% names(available_data)))

  result <- edgar_source$get("NMVOC", "Residential and other sectors", 2022, iso3 = "IDN")
  expect_null(result)
})
