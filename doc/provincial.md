# Methodology for Extracting Provincial Emission Data
## Overview
This document outlines the methodology used to extract monthly spatial emissions data at the provincial level, incorporating custom spatial adjustments to account for coastal areas.

## Data Collection
### Emission Data Source

We collect monthly spatial emission data from URL https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/. This data is available for a variety of pollutants, including NOx, BC, CH4, CO, CO2, N2O, NH3, NMVOC, OC, and SO2. The emissions data is provided in units of kg m-2 s-1 at a resolution of 0.1 degrees.

### Provincial Boundaries

We use GADM4 (Global Administrative Areas) provincial boundary data for the specified country.

## Data Processing
In coastal regions, provinces are extended into the sea by 20 kilometers to account for emissions that may affect neighboring pixels beyond land areas. This is done by creating a buffer zone around the provincial boundaries without overlapping with other provinces.

The emissions are then extracted for each province and pollutant using the buffered boundaries. The extraction process takes the monthly emission data (in units of kg m-2 s-1), multiplies it by the area of each raster cell, and computes the annual emissions in kilotonnes per year.


## Validation Against CEDS National Data
To ensure the accuracy of the extracted emissions data, we validate the sum of the provincial emissions against CEDS national-level emission inventories for each pollutant and year. The results are compared, and the figures are found to be within a 2% difference from the national totals.

This difference could be attributed to emissions that cannot be spatially allocated to a specific province, such as in shipping and aviation.