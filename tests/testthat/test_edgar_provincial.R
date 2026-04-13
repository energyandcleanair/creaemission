test_that("EDGAR provincial constructor uses isolated data and map paths offline", {
  paths <- new_test_source_paths("edgar", include_map = TRUE)

  edgar_provincial <- EDGARProvincial$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir,
    map_data_dir = paths$map_data_dir,
    map_cache_dir = paths$map_cache_dir
  )

  expect_false(edgar_provincial$use_prebuilt_available_data)
  expect_false(edgar_provincial$map_source$use_prebuilt_available_data)
  expect_equal(normalizePath(edgar_provincial$data_dir), normalizePath(paths$data_dir))
  expect_equal(normalizePath(edgar_provincial$cache_dir), normalizePath(paths$cache_dir))
  expect_equal(normalizePath(edgar_provincial$map_source$data_dir), normalizePath(paths$map_data_dir))
  expect_equal(normalizePath(edgar_provincial$map_source$cache_dir), normalizePath(paths$map_cache_dir))
  expect_true(inherits(edgar_provincial, "EDGARProvincial"))
  expect_true(inherits(edgar_provincial, "SourceProvincial"))

  expect_equal(edgar_provincial$clear(), 0)

  available_data <- edgar_provincial$list_available_data()
  expect_equal(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year", "iso3") %in% names(available_data)))

  result <- edgar_provincial$get("NMVOC", "RCO", 2022, "IDN")
  expect_null(result)
})

test_that("EDGAR provincial reuses cached province boundaries", {
  paths <- new_test_source_paths("edgar", include_map = TRUE)

  edgar_provincial <- EDGARProvincial$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir,
    map_data_dir = paths$map_data_dir,
    map_cache_dir = paths$map_cache_dir
  )

  cache_file <- edgar_provincial$get_boundary_cache_path("IN", level = 1, res = "low", buffer_into_sea_km = 20)
  terra::writeVector(terra::as.polygons(terra::ext(0, 1, 0, 1)), cache_file, overwrite = TRUE)

  vect <- edgar_provincial$get_province_boundaries("IN", level = 1, res = "low", buffer_into_sea_km = 20)

  expect_true(file.exists(cache_file))
  expect_equal(nrow(vect), 1)
})
