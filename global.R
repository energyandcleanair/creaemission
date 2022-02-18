library(mongolite)
library(dbplyr)
library(tibble)
library(tidyverse)
library(countrycode)
library(colorRamps)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(lubridate)
library(scales)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras2)
library(plotly)
library(DT)
library(scales)
library(raster)
library(mapview)

sel <- dplyr::select
source('data.R')
# source('utils.R')




# density_location_dates <- db.get_feature_location_dates()
# results_locations <- db.get_results_locations()
# indicators <- c("emission_no2_kg_hr", "mass_no2_kg", "lifetime_hr", "r2")
# emission_methods <- db.get_results_methods()
# # emissions_locations <- db.get_emission_locations(with_emission_data=T)
# results_locations <- results_locations %>%
#   inner_join(emissions_locations)


pollutants <- c("NOx"="nox","SO2"="so2","CH4"="ch4","CO2"="co2")
color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
# chart_types <- c("Bar (Horizontal)"="barh",
#                  "Bar (Vertical)"="barv",
#                  "Lines"="line",
#                  "Areas"="area")
chart_types <- c("Bar (Horizontal)"="barh")
topn <- 20 # How many rows max in chart

ceds_sectors <- c("Total"="total", "Agriculture"="agri", "Energy"="energy",
                  "Industrial"="industrial", "Transportation"="transport",
                  "Residential, Commercial, Other"="rescom",
                  "Solvents production and application"="solvents",
                  "Waste"="waste", "International Shipping"="shipping")

map_palettes=c("Viridis"="viridis","OrRd"="OrRdREVERSE","Inferno"="inferno")
