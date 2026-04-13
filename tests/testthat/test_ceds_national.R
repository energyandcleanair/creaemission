test_that("CEDS national constructor uses isolated paths offline", {
  paths <- new_test_source_paths("ceds")

  ceds_source <- CEDSNational$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir
  )

  expect_false(ceds_source$use_prebuilt_available_data)
  expect_equal(normalizePath(ceds_source$data_dir), normalizePath(paths$data_dir))
  expect_equal(normalizePath(ceds_source$cache_dir), normalizePath(paths$cache_dir))

  expect_equal(ceds_source$clear(), 0)

  available_data <- ceds_source$list_available_data()
  expect_equal(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year", "iso3") %in% names(available_data)))

  result <- ceds_source$get("NMVOC", "1A4b_Residential", 2022)
  expect_null(result)
})
