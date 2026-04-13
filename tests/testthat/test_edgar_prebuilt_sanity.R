test_that("EDGAR prebuilt provincial and national values are matching", {
  skip_if_missing_prebuilt_data(file.path("edgar", "national"), file.path("edgar", "provincial"), file.path("edgar", "maps"))

  national_dir <- get_prebuilt_data_path("edgar", "national")
  provincial_dir <- get_prebuilt_data_path("edgar", "provincial")
  map_dir <- get_prebuilt_data_path("edgar", "maps")

  edgar_national <- EDGARNational$new(
    data_dir = national_dir,
    use_prebuilt_available_data = FALSE
  )
  edgar_provincial <- EDGARProvincial$new(
    data_dir = provincial_dir,
    map_data_dir = map_dir,
    use_prebuilt_available_data = FALSE
  )

  provincial_cases <- PREBUILT_REFERENCE_VALUES$edgar$provincial

  for (case in provincial_cases) {
    national_data <- edgar_national$get(
      pollutant = "NMVOC",
      year = case$year,
      iso3 = case$iso3
    ) %>%
      dplyr::filter(sector_group == case$sector_group)

    provincial_data <- edgar_provincial$get(
      pollutant = "NMVOC",
      year = case$year,
      iso3 = toupper(case$iso3)
    ) %>%
      dplyr::filter(sector_group == case$sector_group)

    expect_false(is.null(national_data), info = case$iso3)
    expect_false(is.null(provincial_data), info = case$iso3)
    expect_true(nrow(national_data) > 0, info = case$iso3)
    expect_true(nrow(provincial_data) > 0, info = case$iso3)

    national_sum_kt <- sum(national_data$value, na.rm = TRUE)
    provincial_sum_kt <- sum(provincial_data$value, na.rm = TRUE)

    expect_equal(
      national_sum_kt,
      case$reference_sum_kt,
      tolerance = case$reference_tolerance,
      info = case$reference_origin
    )
    expect_equal(
      provincial_sum_kt,
      case$reference_sum_kt,
      tolerance = case$reference_tolerance,
      info = case$reference_origin
    )
    expect_equal(
      provincial_sum_kt,
      national_sum_kt,
      tolerance = case$comparison_tolerance,
      info = case$iso3
    )
    expect_true(national_sum_kt > 0, info = case$iso3)
    expect_true(provincial_sum_kt > 0, info = case$iso3)
  }
})

test_that("EDGAR prebuilt national and raster values are matching", {
  skip_if_missing_prebuilt_data(file.path("edgar", "national"), file.path("edgar", "maps"))

  national_dir <- get_prebuilt_data_path("edgar", "national")
  map_dir <- get_prebuilt_data_path("edgar", "maps")

  edgar_national <- EDGARNational$new(
    data_dir = national_dir,
    use_prebuilt_available_data = FALSE
  )
  edgar_map <- EDGARMap$new(
    data_dir = map_dir,
    use_prebuilt_available_data = FALSE
  )

  raster_cases <- PREBUILT_REFERENCE_VALUES$edgar$raster

  for (case in raster_cases) {
    national_iso3 <- if (case$iso3 == "wld") NULL else case$iso3
    national_data <- edgar_national$get(
      pollutant = "NMVOC",
      year = case$year,
      iso3 = national_iso3
    ) %>%
      dplyr::filter(sector_group == case$sector_group)

    expect_false(is.null(national_data), info = case$iso3)
    expect_true(nrow(national_data) > 0, info = case$iso3)

    national_sum_kt <- sum(national_data$value, na.rm = TRUE)

    global_raster <- edgar_map$get_cog(
      pollutant = "NMVOC",
      sector = case$map_sector,
      year = case$year,
      iso3 = "wld"
    )
    expect_false(is.null(global_raster), info = case$iso3)
    expect_equal(terra::units(global_raster), "kg m-2 yr-1", info = case$iso3)

    raster_sum_kt <- extract_raster_national_sum_kt(global_raster, case$iso3)

    expect_equal(
      national_sum_kt,
      case$reference_sum_kt,
      tolerance = case$reference_tolerance,
      info = case$reference_origin
    )
    expect_equal(
      raster_sum_kt,
      case$reference_sum_kt,
      tolerance = case$reference_tolerance,
      info = case$reference_origin
    )
    expect_equal(
      raster_sum_kt,
      national_sum_kt,
      tolerance = case$comparison_tolerance,
      info = case$iso3
    )
    expect_true(raster_sum_kt > 0, info = case$iso3)
    expect_true(national_sum_kt > 0, info = case$iso3)
  }
})
