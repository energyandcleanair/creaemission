test_that("EDGAR map constructor uses isolated paths offline", {
  paths <- new_test_source_paths("edgar")

  edgar_map <- EDGARMap$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir
  )

  expect_false(edgar_map$use_prebuilt_available_data)
  expect_equal(normalizePath(edgar_map$data_dir), normalizePath(paths$data_dir))
  expect_equal(normalizePath(edgar_map$cache_dir), normalizePath(paths$cache_dir))
  expect_true(inherits(edgar_map, "EDGARMap"))
  expect_true(inherits(edgar_map, "SourceMap"))

  expect_equal(edgar_map$clear(), 0)

  available_data <- edgar_map$list_available_data()
  expect_equal(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year") %in% names(available_data)))

  result <- edgar_map$get("NMVOC", "RCO", 2022, "IDN")
  expect_null(result)
})
