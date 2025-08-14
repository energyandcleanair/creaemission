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

## Contact

**Data Team**: [data-team@energyandcleanair.org](mailto:data-team@energyandcleanair.org)
