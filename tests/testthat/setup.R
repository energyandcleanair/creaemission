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

# Create test data directory if it doesn't exist
test_data_dir <- "tests/testthat/test_data"
if (!dir.exists(test_data_dir)) {
  dir.create(test_data_dir, recursive = TRUE)
}

# Set up test environment
Sys.setenv(TESTING = "TRUE")
Sys.setenv(TESTTHAT = "true")
