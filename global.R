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
library(rgdal)
library(glue)
sel <- dplyr::select

# source('emission.R')
# source('data_availability.R')


pollutants <- c("NOx"="NOx",
                "SO2"="SO2",
                "CH4"="CH4",
                "CO2"="CO2",
                "NH3"="NH3",
                "NMVOC"="NMVOC",
                "BC"="BC",
                "CO"="CO",
                "N2O"="N2O"
)
color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")

chart_types <- c("Bar (Horizontal)"="barh",
                 "Timeseries (area)"="area")
# topn <- 20 # How many rows max in chart

ceds_sectors <- c("Total"="total", "Agriculture"="agri", "Energy"="energy",
                  "Industrial"="industrial", "Transportation"="transport",
                  "Residential, Commercial, Other"="rescom",
                  "Solvents production and application"="solvents",
                  "Waste"="waste", "International Shipping"="shipping")

map_palettes=c("Viridis"="viridis","OrRd"="OrRdREVERSE","Inferno"="inferno")

