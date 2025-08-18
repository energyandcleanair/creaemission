#' @title EDGAR Sector Mappings
#' @description Clean sector code to name mappings for EDGAR data source

# EDGAR Pollutants
EDGAR_POLLUTANTS <- c(
  "NOx" = "NOx",
  "SO2" = "SO2",
  "PM25" = "PM2.5",
  "PM10" = "PM10",
  "OC" = "OC",
  "NH3" = "NH3",
  "NMVOC" = "NMVOC"
)

# =============================================================================
# EDGAR NATIONAL MAPPINGS
# =============================================================================

# Raw sector codes to readable sector names
EDGAR_NATIONAL_SECTOR_MAPPING <- c(
  "Civil Aviation" = "Civil Aviation",
  "Glass Production" = "Glass Production",
  "Main Activity Electricity and Heat Production" = "Power Generation",
  "Manufacturing Industries and Construction" = "Manufacturing & Construction",
  "Non-Specified" = "Non-Specified",
  "Other Process Uses of Carbonates" = "Carbonate Processing",
  "Other Transportation" = "Other Transportation",
  "Residential and other sectors" = "Residential & Other",
  "Solid Fuels" = "Solid Fuels",
  "Water-borne Navigation" = "Maritime Transport",
  "Cement production" = "Cement Production",
  "Chemical Industry" = "Chemical Industry",
  "Emissions from biomass burning" = "Biomass Burning",
  "Oil and Natural Gas" = "Oil & Natural Gas",
  "Petroleum Refining - Manufacture of Solid Fuels and Other Energy Industries" = "Petroleum Refining",
  "Lime production" = "Lime Production",
  "Metal Industry" = "Metal Industry",
  "Other" = "Other",
  "Incineration and Open Burning of Waste" = "Waste Incineration",
  "Railways" = "Railways",
  "Fossil fuel fires" = "Fossil Fuel Fires",
  "Direct N2O Emissions from managed soils" = "Managed Soils",
  "Manure Management" = "Manure Management",
  "Wastewater Treatment and Discharge" = "Wastewater Treatment",
  "Non-Energy Products from Fuels and Solvent Use" = "Solvent Use",
  "Biological Treatment of Solid Waste" = "Biological Waste Treatment",
  "Other Product Manufacture and Use" = "Other Product Use",
  "Solid Waste Disposal" = "Solid Waste Disposal",
  "Liming" = "Liming",
  "Rice cultivations" = "Rice Cultivation",
  "Road Transportation resuspension" = "Road Transport",
  "Road Transportation no resuspension" = "Road Transport"
)

# Readable sector names to sector groups
EDGAR_NATIONAL_SECTOR_GROUP_MAPPING <- c(
  "Civil Aviation" = "Transport",
  "Glass Production" = "Industry",
  "Power Generation" = "Power Generation",
  "Manufacturing & Construction" = "Industry",
  "Non-Specified" = "Other",
  "Carbonate Processing" = "Industry",
  "Other Transportation" = "Transport",
  "Residential & Other" = "Buildings",
  "Solid Fuels" = "Power Generation",
  "Maritime Transport" = "Transport",
  "Cement Production" = "Industry",
  "Chemical Industry" = "Industry",
  "Biomass Burning" = "Other",
  "Oil & Natural Gas" = "Power Generation",
  "Petroleum Refining" = "Industry",
  "Lime Production" = "Industry",
  "Metal Industry" = "Industry",
  "Other" = "Other",
  "Waste Incineration" = "Waste",
  "Railways" = "Transport",
  "Fossil Fuel Fires" = "Other",
  "Managed Soils" = "Agriculture",
  "Manure Management" = "Agriculture",
  "Wastewater Treatment" = "Waste",
  "Solvent Use" = "Industry",
  "Biological Waste Treatment" = "Waste",
  "Other Product Use" = "Industry",
  "Solid Waste Disposal" = "Waste",
  "Liming" = "Agriculture",
  "Rice Cultivation" = "Agriculture",
  "Road Transportation resuspension" = "Transport",
  "Road Transportation no resuspension" = "Transport",
  "Road Transport" = "Transport"
)

# =============================================================================
# EDGAR PROVINCIAL MAPPINGS
# =============================================================================

# Raw sector codes to readable sector names
EDGAR_PROVINCIAL_SECTOR_MAPPING <- c(
  "ENE" = "Energy",
  "REF" = "Refineries",
  "IND" = "Industry",
  "TNR" = "Transport Road",
  "TRO" = "Transport Other",
  "RCO" = "Residential & Commercial",
  "PRO" = "Production",
  "NMM" = "Non-Metallic Minerals",
  "CHE" = "Chemical Industry",
  "IRO" = "Iron & Steel",
  "NFE" = "Non-Ferrous Metals",
  "FOO" = "Food & Beverage",
  "MNM" = "Mining",
  "AWB" = "Agricultural Waste Burning",
  "AGS" = "Agriculture",
  "SWD" = "Solid Waste Disposal",
  "NEU" = "Non-Energy Use",
  "PRU" = "Product Use",
  "WWT" = "Wastewater Treatment"
)

# Readable sector names to sector groups
EDGAR_PROVINCIAL_SECTOR_GROUP_MAPPING <- c(
  "Energy" = "Power Generation",
  "Refineries" = "Industry",
  "Industry" = "Industry",
  "Transport Road" = "Transport",
  "Transport Other" = "Transport",
  "Residential & Commercial" = "Buildings",
  "Production" = "Industry",
  "Non-Metallic Minerals" = "Industry",
  "Chemical Industry" = "Industry",
  "Iron & Steel" = "Industry",
  "Non-Ferrous Metals" = "Industry",
  "Food & Beverage" = "Industry",
  "Mining" = "Industry",
  "Agricultural Waste Burning" = "Agriculture",
  "Agriculture" = "Agriculture",
  "Solid Waste Disposal" = "Waste",
  "Non-Energy Use" = "Industry",
  "Product Use" = "Industry",
  "Wastewater Treatment" = "Waste"
)
