test_that("CEDS provincial constructor uses isolated data and map paths offline", {
  paths <- new_test_source_paths("ceds", include_map = TRUE)

  ceds_provincial <- CEDSProvincial$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir,
    map_data_dir = paths$map_data_dir,
    map_cache_dir = paths$map_cache_dir
  )

  expect_false(ceds_provincial$use_prebuilt_available_data)
  expect_false(ceds_provincial$map_source$use_prebuilt_available_data)
  expect_equal(normalizePath(ceds_provincial$data_dir), normalizePath(paths$data_dir))
  expect_equal(normalizePath(ceds_provincial$cache_dir), normalizePath(paths$cache_dir))
  expect_equal(normalizePath(ceds_provincial$map_source$data_dir), normalizePath(paths$map_data_dir))
  expect_equal(normalizePath(ceds_provincial$map_source$cache_dir), normalizePath(paths$map_cache_dir))

  expect_equal(ceds_provincial$clear(), 0)

  available_data <- ceds_provincial$list_available_data()
  expect_equal(nrow(available_data), 0)
  expect_true(all(c("pollutant", "sector", "year", "iso3") %in% names(available_data)))

  result <- ceds_provincial$get("NMVOC", "Residential, Commercial, Other", 2022, "IDN")
  expect_null(result)
})

test_that("CEDS provincial reuses cached province boundaries", {
  workspace_root <- new_test_workspace("ceds-cache")
  paths <- list(
    data_dir = ensure_dir(file.path(workspace_root, "data")),
    cache_dir = ensure_dir(file.path(workspace_root, "cache")),
    map_data_dir = ensure_dir(file.path(workspace_root, "maps")),
    map_cache_dir = ensure_dir(file.path(workspace_root, "map-cache"))
  )

  ceds_provincial <- CEDSProvincial$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir,
    map_data_dir = paths$map_data_dir,
    map_cache_dir = paths$map_cache_dir
  )

  cache_file <- ceds_provincial$get_boundary_cache_path("IN", level = 1, res = "low", buffer_into_sea_km = 20)
  unlink(cache_file, force = TRUE)
  terra::writeVector(terra::as.polygons(terra::ext(0, 1, 0, 1)), cache_file, overwrite = TRUE)

  vect <- ceds_provincial$get_province_boundaries("IN", level = 1, res = "low", buffer_into_sea_km = 20)

  expect_true(file.exists(cache_file))
  expect_equal(nrow(vect), 1)
})

test_that("CEDS provincial sea buffer keeps synthetic provinces off foreign land", {
  skip_if_not_installed("sf")
  skip_if_not_installed("cartomisc")

  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  paths <- new_test_source_paths("ceds", include_map = TRUE)

  ceds_provincial <- CEDSProvincial$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir,
    map_data_dir = paths$map_data_dir,
    map_cache_dir = paths$map_cache_dir
  )

  province_polygons <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(102.00, 12.40),
      c(102.40, 12.40),
      c(102.40, 13.00),
      c(102.00, 13.00),
      c(102.00, 12.40)
    ))),
    sf::st_polygon(list(rbind(
      c(100.60, 12.00),
      c(101.30, 12.00),
      c(101.30, 12.70),
      c(100.60, 12.70),
      c(100.60, 12.00)
    ))),
    crs = 4326
  )

  province_sf <- sf::st_sf(
    GID_0 = c("THA", "THA"),
    GID_1 = c("THA.synthetic.1", "THA.synthetic.2"),
    NAME_1 = c("Synthetic East", "Synthetic Gulf"),
    geometry = province_polygons
  )

  buffered_vect <- ceds_provincial$buffer_into_sea(
    terra::vect(province_sf),
    id_col = "GID_1",
    buffer_into_sea_km = 200,
    res = "low"
  )

  buffered_sf <- sf::st_as_sf(buffered_vect) %>%
    sf::st_make_valid()

  original_projected <- sf::st_transform(province_sf, 3857)
  buffered_projected <- sf::st_transform(buffered_sf, 3857)

  original_by_id <- split(original_projected, original_projected$GID_1)
  buffered_by_id <- split(buffered_projected, buffered_projected$GID_1)

  for (province_id in names(original_by_id)) {
    original_geom <- sf::st_geometry(original_by_id[[province_id]])
    buffered_geom <- sf::st_geometry(buffered_by_id[[province_id]])
    uncovered_area <- suppressWarnings(sf::st_difference(original_geom, buffered_geom))
    expect_lt(sum(as.numeric(sf::st_area(uncovered_area)), na.rm = TRUE), 1)
  }

  foreign_land <- suppressWarnings(
    creahelpers::get_adm(level = 0, res = "low") %>%
      terra::vect() %>%
      sf::st_as_sf() %>%
      dplyr::filter(GID_0 != "THA") %>%
      sf::st_transform(3857) %>%
      sf::st_make_valid() %>%
      sf::st_crop(sf::st_as_sfc(sf::st_bbox(buffered_projected)))
  )

  added_buffer <- suppressWarnings(
    sf::st_difference(
      sf::st_union(buffered_projected),
      sf::st_union(original_projected)
    )
  )
  foreign_overlap <- suppressWarnings(sf::st_intersection(added_buffer, sf::st_union(foreign_land)))
  overlap_area <- sum(as.numeric(sf::st_area(foreign_overlap)), na.rm = TRUE)

  expect_equal(sort(buffered_sf$GID_1), sort(province_sf$GID_1))
  expect_lt(overlap_area, 10)
})

test_that("CEDS provincial Thailand boundaries do not overlap foreign land after sea buffering", {
  skip_if_not_installed("sf")
  skip_if_not_installed("cartomisc")

  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  paths <- new_test_source_paths("ceds", include_map = TRUE)

  ceds_provincial <- CEDSProvincial$new(
    data_dir = paths$data_dir,
    cache_dir = paths$cache_dir,
    map_data_dir = paths$map_data_dir,
    map_cache_dir = paths$map_cache_dir
  )

  vect <- ceds_provincial$get_province_boundaries("TH", level = 1, res = "low", buffer_into_sea_km = 20)
  buffered_sf <- sf::st_as_sf(vect) %>%
    sf::st_make_valid()
  original_sf <- terra::vect(creahelpers::get_adm(level = 1, res = "low", iso2s = "TH")) %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()
  buffered_projected <- sf::st_transform(buffered_sf, 3857)
  original_projected <- sf::st_transform(original_sf, 3857)

  foreign_land <- suppressWarnings(
    creahelpers::get_adm(level = 0, res = "low") %>%
      terra::vect() %>%
      sf::st_as_sf() %>%
      dplyr::filter(GID_0 != "THA") %>%
      sf::st_transform(3857) %>%
      sf::st_make_valid() %>%
      sf::st_crop(sf::st_as_sfc(sf::st_bbox(buffered_projected)))
  )

  added_buffer <- suppressWarnings(
    sf::st_difference(
      sf::st_union(buffered_projected),
      sf::st_union(original_projected)
    )
  )
  foreign_overlap <- suppressWarnings(sf::st_intersection(added_buffer, sf::st_union(foreign_land)))
  overlap_area <- sum(as.numeric(sf::st_area(foreign_overlap)), na.rm = TRUE)

  expect_lt(overlap_area, 10)
})
