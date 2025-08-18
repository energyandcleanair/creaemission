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

# Verify we're in the right place and using the same cache
project_root <- get_project_root()
cache_folder <- get_cache_folder()
message(glue::glue("Test setup - Project root: {project_root}"))
message(glue::glue("Test setup - Cache folder: {cache_folder}"))

# Test data directory will be created on-demand by get_test_data_path

# Set up test environment
Sys.setenv(TESTING = "TRUE")
Sys.setenv(TESTTHAT = "true")

# Function to get test data path (variadic segments; creates dirs on demand)
get_test_data_path <- function(...) {
  path <- file.path(project_root, "tests", "testthat", "test_data", ...)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

# Make the function available in test environment
assign("get_test_data_path", get_test_data_path, envir = .GlobalEnv)
