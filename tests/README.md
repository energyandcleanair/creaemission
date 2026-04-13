# Testing Framework

This directory contains the `testthat` suite for `creaemission`.

## Running Tests

### Run all tests
```r
devtools::test()
```

### Run a specific file
```r
testthat::test_file("tests/testthat/test_ceds_prebuilt_sanity.R")
```

### Run the active file
```r
devtools::test_active_file()
```

## Test Structure

### Constructor and offline smoke tests
- These verify that source constructors can be created with isolated paths.
- They check `clear()`, empty `list_available_data()`, and `get()` on fresh directories.
- They do not call `build()` and should be safe to run on CI without network access.

### Prebuilt sanity tests
- These are read-only checks against prebuilt files under `data/...` when those directories are available locally.
- They cover:
  - national vs provincial consistency
  - national vs raster consistency
- Raster country checks derive national totals from the world raster using `creahelpers::get_adm()` and `terra::extract()`.
- Manually curated reference sums used by these checks live in `tests/testthat/helper_prebuilt_reference_values.R`.
- That helper also stores per-case tolerances, so countries can use different thresholds.
- On environments without prebuilt data, such as GitHub runners, these tests skip cleanly.

### Test cache helpers
- The suite uses `tests/testthat/cache/` as a repo-local test cache root.
- Cache seeding can reuse matching artifacts from the main `cache/` tree by hard-linking or copying them into the test cache.
- This speeds up local rebuild-oriented workflows without making the default suite depend on downloads.

## Test Data and Paths

- `tests/testthat/test_data/` holds isolated temporary workspaces created during tests.
- `tests/testthat/cache/` holds reusable test cache artifacts.
- Prebuilt sanity tests read from `data/...` and never mutate those directories.
