test_that("CEDS map constructor uses isolated paths offline", {
  paths <- new_test_source_paths("ceds")

  ceds_map <- CEDSMap$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir
  )

  expect_false(ceds_map$use_prebuilt_available_data)
  expect_equal(normalizePath(ceds_map$data_dir), normalizePath(paths$data_dir))
  expect_equal(normalizePath(ceds_map$cache_dir), normalizePath(paths$cache_dir))

  expect_equal(ceds_map$clear(), 0)

  available_data <- ceds_map$list_available_data()
  expect_equal(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year") %in% names(available_data)))

  result <- ceds_map$get("SO2", "Energy", 2022, "IDN")
  expect_null(result)
})
