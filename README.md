# CREA Emission Portal

An R package to collect and compute emission figures and maps from various sources, with a Shiny app to browse the results.

**Live Demo**: [emission.energyandcleanair.org](https://emission.energyandcleanair.org)

## What This Is

- **R Package**: Collects and computes emission figures and maps from various sources (CEDS, EDGAR)
- **Shiny App**: Interactive web interface for exploring emissions data with charts and maps
- **Data Sources**: Support for national and provincial-level emissions analysis

## Code Structure

The package is built around three core R6 classes:

### `SourceNational`
- Handles country-level emissions data
- Provides methods for building, listing, and retrieving national emissions data
- Base class for CEDS and EDGAR national data sources

### `SourceProvincial` 
- Manages sub-national (provincial) emissions data
- Extracts provincial data from gridded emissions data
- Integrates with map sources for spatial analysis

### `SourceMap`
- Handles spatial emissions data and raster operations
- Manages NetCDF files and provides map raster objects
- Includes country boundary cropping and masking functionality

## Build Data

Before running the Shiny app, you need to build the emissions data first. This process downloads and processes data from various sources.

```r
# Build all emissions data (including maps)
source("build_data.R")

# Or build data programmatically
build_all_emissions_data(build_maps = TRUE)
```

For detailed build options and configuration, see [build_data.R](build_data.R).

## Run Locally

First build the package:

```r
devtools::install()
```
Then run the Shiny app:

```r
# Run the Shiny app
shiny::runApp('inst/')
```

## Deploy

For deployment instructions, see [DEPLOY.md](DEPLOY.md).

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
  - comparison with some hardcoded/manually extracted values
- On environments without prebuilt data, such as GitHub runners, these tests skip cleanly.

### Test cache helpers
- The suite uses `tests/testthat/cache/` as a repo-local test cache root.
- Cache seeding can reuse matching artifacts from the main `cache/` tree by hard-linking or copying them into the test cache.
- This speeds up local rebuild-oriented workflows without making the default suite depend on downloads.

## Test Data and Paths

- `tests/testthat/test_data/` holds isolated temporary workspaces created during tests.
- `tests/testthat/cache/` holds reusable test cache artifacts.
- Prebuilt sanity tests read from `data/...` and never mutate those directories.

## Contact

**Data Team**: [data-team@energyandcleanair.org](mailto:data-team@energyandcleanair.org)
