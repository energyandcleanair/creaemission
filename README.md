# CREA Emission Portal

A comprehensive Shiny portal for exploring emissions data from multiple sources including CEDS and EDGAR.

## Features

- **Interactive Charts**: Dynamic charts for national and provincial emissions data
- **Interactive Maps**: Comprehensive mapping capabilities for provincial emissions data
- **Multiple Data Sources**: Support for CEDS, EDGAR, and other emissions databases
- **Provincial Analysis**: Detailed provincial-level emissions analysis and visualization
- **Dynamic Data**: Real-time pollutant availability based on actual data sources

## Map Functionality

The portal now includes comprehensive map functionality for visualizing provincial emissions data:

### Provincial Source Classes

Each provincial source class (CEDSSourceProvincial, EDGARSourceProvincial) includes three key methods:

- **`build_maps()`**: Build TIF files for all available combinations of pollutants, sectors, years, and countries
- **`available_maps()`**: Get a data frame of all available map combinations
- **`get_map()`**: Generate a raster for specific parameters (pollutant, sector, year, country)

### Usage Examples

```r
# Build all maps for CEDS
ceds_provincial <- CEDSSourceProvincial$new()
ceds_provincial$build_maps()

# Get available maps
available_maps <- ceds_provincial$available_maps()

# Generate a specific map
map_raster <- ceds_provincial$get_map(
  pollutant = "NOx",
  sector = "0",  # Agriculture
  year = 2022,
  iso2 = "ID"    # Indonesia
)

# Use the main function
raster <- get_emissions_raster(
  poll = "NOx",
  year = 2022,
  sector = "0",
  source = "CEDS",
  iso2 = "ID"
)
```

### Building Maps

To build maps for the catalog:

```r
# Build maps for all sources
build_emissions_maps()

# Build maps for specific parameters
build_emissions_maps(
  sources = c("CEDS"),
  pollutants = c("NOx", "SO2"),
  years = 2022,
  iso2s = c("ID", "IN")
)
```

## Installation

```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "leaflet", "plotly", "terra"))

# Install from GitHub
remotes::install_github("energyandcleanair/creahelpers")
remotes::install_github("energyandcleanair/rcrea")
```

## Data Sources

- **CEDS**: Community Emissions Data System - Comprehensive emissions inventory
- **EDGAR**: Emissions Database for Global Atmospheric Research - Global atmospheric emissions
- **Future Sources**: Extensible architecture for additional emissions databases

## Usage

```r
# Run the portal
shiny::runApp()

# Build data
build_all_emissions_data(build_maps = TRUE)
```
