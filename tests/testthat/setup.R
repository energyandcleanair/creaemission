# Setup file for tests
# This file is run before tests

# Load required libraries
library(dplyr)
library(tidyr)
library(terra)
library(lubridate)
library(glue)
library(stringr)
library(ncdf4)
library(pbapply)
library(countrycode)

# Load the package for testing
library(creaemission)

# Verify we're in the right place
project_root <- get_project_root()
test_root <- file.path(project_root, "tests", "testthat")
test_data_root <- file.path(test_root, "test_data")
test_cache_root <- file.path(test_root, "cache")
message(glue::glue("Test setup - Project root: {project_root}"))
message(glue::glue("Test setup - Test data root: {test_data_root}"))
message(glue::glue("Test setup - Test cache root: {test_cache_root}"))

# Set up test environment
Sys.setenv(TESTING = "TRUE")
Sys.setenv(TESTTHAT = "true")

# Create a directory if it does not exist and return the path.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

ensure_dir(test_data_root)
ensure_dir(test_cache_root)

# Function to get test data path (variadic segments; creates dirs on demand)
get_test_data_path <- function(...) {
  path <- file.path(test_data_root, ...)
  ensure_dir(path)
  path
}

# Function to get prebuilt data path (variadic segments; does not create dirs)
get_prebuilt_data_path <- function(...) {
  file.path(project_root, "data", ...)
}

# Skip a test if one or more expected prebuilt data directories are missing.
skip_if_missing_prebuilt_data <- function(...) {
  paths <- vapply(list(...), get_prebuilt_data_path, character(1))
  missing <- paths[!dir.exists(paths)]

  if (length(missing) > 0) {
    skip(paste("Missing prebuilt data directories:", paste(missing, collapse = ", ")))
  }
}

# Extract a national total from a world raster by masking against ADM0 boundaries.
extract_raster_national_sum_kt <- function(raster, iso3) {
  raster_kt <- raster * terra::cellSize(raster, unit = "m") / 1e6

  if (tolower(iso3) == "wld") {
    return(sum(raster_kt[], na.rm = TRUE))
  }

  iso2 <- countrycode::countrycode(toupper(iso3), "iso3c", "iso2c")
  if (is.na(iso2)) {
    stop(glue::glue("Could not map ISO3 code '{iso3}' to ISO2"))
  }

  adm0 <- creahelpers::get_adm(level = 0, res = "low", iso2s = iso2)
  boundary <- terra::vect(adm0)
  extracted <- terra::extract(raster_kt, boundary, fun = sum, na.rm = TRUE)

  extracted[[2]][1]
}

# Function to create a fresh repo-local workspace for isolated constructor tests
new_test_workspace <- function(prefix = "workspace") {
  path <- tempfile(pattern = paste0(prefix, "-"), tmpdir = test_data_root)
  ensure_dir(path)
}

# Hard-link a file into the test cache when possible; otherwise copy it.
link_or_copy_file <- function(source, destination) {
  ensure_dir(dirname(destination))

  if (file.exists(destination)) {
    return(destination)
  }

  linked <- suppressWarnings(file.link(source, destination))
  if (isTRUE(linked)) {
    return(destination)
  }

  copied <- file.copy(source, destination, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
  if (!isTRUE(copied)) {
    stop(glue::glue("Failed to seed test cache from {source} to {destination}"))
  }

  destination
}

# Seed a test cache path from the repo cache using the same relative path.
seed_test_cache_path <- function(...) {
  relative_path <- file.path(...)
  source_path <- file.path(project_root, "cache", relative_path)
  destination_path <- file.path(test_cache_root, relative_path)

  if (file.exists(source_path)) {
    return(link_or_copy_file(source_path, destination_path))
  }

  if (!dir.exists(source_path)) {
    return(destination_path)
  }

  ensure_dir(destination_path)

  entries <- list.files(
    source_path,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    full.names = FALSE,
    include.dirs = TRUE
  )

  for (entry in entries) {
    source_entry <- file.path(source_path, entry)
    destination_entry <- file.path(destination_path, entry)

    if (dir.exists(source_entry)) {
      ensure_dir(destination_entry)
    } else {
      link_or_copy_file(source_entry, destination_entry)
    }
  }

  destination_path
}

# Function to get test cache path (variadic segments; creates dirs on demand)
get_test_cache_path <- function(..., seed = FALSE) {
  path <- file.path(test_cache_root, ...)
  ensure_dir(path)

  if (seed) {
    seed_test_cache_path(...)
  }

  path
}

# Helper to build a fresh isolated source workspace while keeping cache reusable.
new_test_source_paths <- function(source, include_map = FALSE, seed_cache = FALSE) {
  workspace_root <- new_test_workspace(source)

  paths <- list(
    workspace_root = workspace_root,
    data_dir = ensure_dir(file.path(workspace_root, "data")),
    cache_dir = get_test_cache_path(source, seed = seed_cache)
  )

  if (isTRUE(include_map)) {
    paths$map_data_dir <- ensure_dir(file.path(workspace_root, "maps"))
    paths$map_cache_dir <- get_test_cache_path(source, seed = seed_cache)
  }

  paths
}

# Make the helpers available in the test environment
assign("get_test_data_path", get_test_data_path, envir = .GlobalEnv)
assign("get_prebuilt_data_path", get_prebuilt_data_path, envir = .GlobalEnv)
assign("skip_if_missing_prebuilt_data", skip_if_missing_prebuilt_data, envir = .GlobalEnv)
assign("extract_raster_national_sum_kt", extract_raster_national_sum_kt, envir = .GlobalEnv)
assign("get_test_cache_path", get_test_cache_path, envir = .GlobalEnv)
assign("seed_test_cache_path", seed_test_cache_path, envir = .GlobalEnv)
assign("new_test_workspace", new_test_workspace, envir = .GlobalEnv)
assign("new_test_source_paths", new_test_source_paths, envir = .GlobalEnv)
