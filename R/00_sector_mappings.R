#' @title Sector Mappings
#' @description Clean sector code to name mappings for each data source
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble

# CEDS Sector Mappings
CEDS_POLLUTANTS <- c(
  "NOx" = "NOx",
  "BC" = "BC",
  "CH4" = "CH4",
  "CO" = "CO",
  "CO2" = "CO2",
  "N2O" = "N2O",
  "NH3" = "NH3",
  "NMVOC" = "NMVOC",
  "OC" = "OC",
  "SO2" = "SO2"
)

#' @export
# CEDS_NATIONAL_SECTORS <- c(
#   "1A1a_Electricity-public" = "Energy",
#   "1A1b_Industry" = "Energy",
#   "1A1c_Other-transformation" = "Energy",
#   "1A2_Manufacturing" = "Industrial",
#   "1A3_Transport" = "Transportation",
#   "1A4a_Commercial-institutional" = "Residential, Commercial, Other",
#   "1A4b_Residential" = "Residential, Commercial, Other",
#   "1A4c_Agriculture-forestry-fishing" = "Agriculture",
#   "1A5_Other-fuel-combustion" = "Energy",
#   "2A1_Cement-production" = "Industrial",
#   "2A2_Lime-production" = "Industrial",
#   "2A3_Glass-production" = "Industrial",
#   "2A4_Other-minerals" = "Industrial",
#   "2B1_Ammonia" = "Industrial",
#   "2B2_Nitric-acid" = "Industrial",
#   "2B3_Adipic-acid" = "Industrial",
#   "2B4_Caprolactam" = "Industrial",
#   "2B5_Carbide" = "Industrial",
#   "2B6_Titanium-dioxide" = "Industrial",
#   "2B7_Soda-ash" = "Industrial",
#   "2B8_Petrochemicals" = "Industrial",
#   "2B9_Other-chemicals" = "Industrial",
#   "2C1_Iron-steel" = "Industrial",
#   "2C2_Non-ferrous-metals" = "Industrial",
#   "2C3_Chemicals" = "Industrial",
#   "2C4_Pulp-paper" = "Industrial",
#   "2C5_Food-beverages" = "Industrial",
#   "2C6_Other-processes" = "Industrial",
#   "2D1_Landfills" = "Waste",
#   "2D2_Wastewater" = "Waste",
#   "2D3_Waste-incineration" = "Waste",
#   "2D4_Other-waste" = "Waste",
#   "2E_Solvents" = "Solvents production and application",
#   "2F_Product-uses" = "Industrial",
#   "2G_Other-product-use" = "Industrial",
#   "3A_Enteric-fermentation" = "Agriculture",
#   "3B_Manure-management" = "Agriculture",
#   "3C_Rice-cultivation" = "Agriculture",
#   "3D_Agricultural-soils" = "Agriculture",
#   "3E_Agricultural-waste-burning" = "Agriculture",
#   "3F_Other-agriculture" = "Agriculture",
#   "4A_Solid-waste-disposal" = "Waste",
#   "4B_Biological-treatment-solid-waste" = "Waste",
#   "4C_Wastewater-handling" = "Waste",
#   "4D_Waste-incineration" = "Waste",
#   "4E_Other-waste" = "Waste",
#   "5A_International-aviation" = "Transportation",
#   "5B_International-shipping" = "International Shipping"
# )

# EDGAR Sector Mappings
#' @export
# EDGAR_NATIONAL_SECTORS <- c(
#   "Energy" = "Energy",
#   "Industry" = "Industry",
#   "Transport" = "Transport",
#   "Residential and other sectors" = "Residential and other sectors",
#   "Agriculture" = "Agriculture",
#   "Waste" = "Waste",
#   "International shipping" = "International shipping",
#   "International aviation" = "International aviation"
#   # "Total" = "Total"
# )

# EDGAR_POLLUTANTS = c("BC", "CO", "NH3", "NMVOC", "NOX", "OC", "PM10", "PM2.5", "SO2"),
EDGAR_POLLUTANTS = c(
  "NOx"="NOx",
  "SO2"="SO2",
  "PM25" = "PM2.5",
  "PM10" = "PM10",
  "OC" = "OC",
  "NH3" = "NH3",
  "NMVOC" = "NMVOC"
  )

# Sector mappings for different data sources
# Two-tier system: Raw -> Readable -> Group

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

# =============================================================================
# CEDS NATIONAL MAPPINGS
# =============================================================================

# Raw sector codes to readable sector names
CEDS_NATIONAL_SECTOR_MAPPING <- c(
  "1A1a_Electricity-autoproducer" = "Electricity Autoproducer",
  "1A1a_Electricity-public" = "Electricity Public",
  "1A1a_Heat-production" = "Heat Production",
  "1A1bc_Other-transformation" = "Other Transformation",
  "1A2a_Ind-Comb-Iron-steel" = "Iron & Steel Combustion",
  "1A2b_Ind-Comb-Non-ferrous-metals" = "Non-Ferrous Metals Combustion",
  "1A2c_Ind-Comb-Chemicals" = "Chemicals Combustion",
  "1A2d_Ind-Comb-Pulp-paper" = "Pulp & Paper Combustion",
  "1A2e_Ind-Comb-Food-tobacco" = "Food & Tobacco Combustion",
  "1A2f_Ind-Comb-Non-metalic-minerals" = "Non-Metallic Minerals Combustion",
  "1A2g_Ind-Comb-Construction" = "Construction Combustion",
  "1A2g_Ind-Comb-machinery" = "Machinery Combustion",
  "1A2g_Ind-Comb-mining-quarying" = "Mining & Quarrying Combustion",
  "1A2g_Ind-Comb-other" = "Other Industry Combustion",
  "1A2g_Ind-Comb-textile-leather" = "Textile & Leather Combustion",
  "1A2g_Ind-Comb-transpequip" = "Transport Equipment Combustion",
  "1A2g_Ind-Comb-wood-products" = "Wood Products Combustion",
  "1A3aii_Domestic-aviation" = "Domestic Aviation",
  "1A3b_Road" = "Road Transport",
  "1A3c_Rail" = "Rail Transport",
  "1A3di_Oil_Tanker_Loading" = "Oil Tanker Loading",
  "1A3dii_Domestic-navigation" = "Domestic Navigation",
  "1A3eii_Other-transp" = "Other Transport",
  "1A4a_Commercial-institutional" = "Commercial & Institutional",
  "1A4b_Residential" = "Residential",
  "1A4c_Agriculture-forestry-fishing" = "Agriculture, Forestry & Fishing",
  "1A5_Other-unspecified" = "Other Unspecified",
  "1B1_Fugitive-solid-fuels" = "Fugitive Solid Fuels",
  "1B2_Fugitive-petr" = "Fugitive Petroleum",
  "1B2b_Fugitive-NG-distr" = "Fugitive Natural Gas Distribution",
  "1B2b_Fugitive-NG-prod" = "Fugitive Natural Gas Production",
  "1B2d_Fugitive-other-energy" = "Fugitive Other Energy",
  "2A1_Cement-production" = "Cement Production",
  "2A2_Lime-production" = "Lime Production",
  "2Ax_Other-minerals" = "Other Minerals",
  "2B_Chemical-industry" = "Chemical Industry",
  "2B2_Chemicals-Nitric-acid" = "Nitric Acid Production",
  "2B3_Chemicals-Adipic-acid" = "Adipic Acid Production",
  "2C1_Iron-steel-alloy-prod" = "Iron & Steel Production",
  "2C3_Aluminum-production" = "Aluminum Production",
  "2C4_Non-Ferrous-other-metals" = "Other Non-Ferrous Metals",
  "2D_Chemical-products-manufacture-processing" = "Chemical Products Processing",
  "2D_Degreasing-Cleaning" = "Degreasing & Cleaning",
  "2D_Other-product-use" = "Other Product Use",
  "2D_Paint-application" = "Paint Application",
  "2H_Pulp-and-paper-food-beverage-wood" = "Pulp, Paper, Food & Wood",
  "3B_Manure-management" = "Manure Management",
  "3D_Rice-Cultivation" = "Rice Cultivation",
  "3D_Soil-emissions" = "Soil Emissions",
  "3E_Enteric-fermentation" = "Enteric Fermentation",
  "3I_Agriculture-other" = "Other Agriculture",
  "5A_Solid-waste-disposal" = "Solid Waste Disposal",
  "5C_Waste-combustion" = "Waste Combustion",
  "5D_Wastewater-handling" = "Wastewater Handling",
  "5E_Other-waste-handling" = "Other Waste Handling",
  "6A_Other-in-total" = "Other In Total",
  "6B_Other-not-in-total" = "Other Not In Total",
  "7A_Fossil-fuel-fires" = "Fossil Fuel Fires",
  "7BC_Indirect-N2O-non-agricultural-N" = "Indirect N2O Non-Agricultural",
  "1A3ai_International-aviation" = "International Aviation",
  "1A3di_International-shipping" = "International Shipping"
)

# Readable sector names to sector groups
CEDS_NATIONAL_SECTOR_GROUP_MAPPING <- c(
  "Electricity Autoproducer" = "Power Generation",
  "Electricity Public" = "Power Generation",
  "Heat Production" = "Power Generation",
  "Other Transformation" = "Power Generation",
  "Iron & Steel Combustion" = "Industry",
  "Non-Ferrous Metals Combustion" = "Industry",
  "Chemicals Combustion" = "Industry",
  "Pulp & Paper Combustion" = "Industry",
  "Food & Tobacco Combustion" = "Industry",
  "Non-Metallic Minerals Combustion" = "Industry",
  "Construction Combustion" = "Industry",
  "Machinery Combustion" = "Industry",
  "Mining & Quarrying Combustion" = "Industry",
  "Other Industry Combustion" = "Industry",
  "Textile & Leather Combustion" = "Industry",
  "Transport Equipment Combustion" = "Industry",
  "Wood Products Combustion" = "Industry",
  "Domestic Aviation" = "Transport",
  "Road Transport" = "Transport",
  "Rail Transport" = "Transport",
  "Oil Tanker Loading" = "Transport",
  "Domestic Navigation" = "Transport",
  "Other Transport" = "Transport",
  "Commercial & Institutional" = "Buildings",
  "Residential" = "Buildings",
  "Agriculture, Forestry & Fishing" = "Agriculture",
  "Other Unspecified" = "Other",
  "Fugitive Solid Fuels" = "Fugitive Emissions",
  "Fugitive Petroleum" = "Fugitive Emissions",
  "Fugitive Natural Gas Distribution" = "Fugitive Emissions",
  "Fugitive Natural Gas Production" = "Fugitive Emissions",
  "Fugitive Other Energy" = "Fugitive Emissions",
  "Cement Production" = "Industry",
  "Lime Production" = "Industry",
  "Other Minerals" = "Industry",
  "Chemical Industry" = "Industry",
  "Nitric Acid Production" = "Industry",
  "Adipic Acid Production" = "Industry",
  "Iron & Steel Production" = "Industry",
  "Aluminum Production" = "Industry",
  "Other Non-Ferrous Metals" = "Industry",
  "Chemical Products Processing" = "Industry",
  "Degreasing & Cleaning" = "Industry",
  "Other Product Use" = "Industry",
  "Paint Application" = "Industry",
  "Pulp, Paper, Food & Wood" = "Industry",
  "Manure Management" = "Agriculture",
  "Rice Cultivation" = "Agriculture",
  "Soil Emissions" = "Agriculture",
  "Enteric Fermentation" = "Agriculture",
  "Other Agriculture" = "Agriculture",
  "Solid Waste Disposal" = "Waste",
  "Waste Combustion" = "Waste",
  "Wastewater Handling" = "Waste",
  "Other Waste Handling" = "Waste",
  "Other In Total" = "Other",
  "Other Not In Total" = "Other",
  "Fossil Fuel Fires" = "Other",
  "Indirect N2O Non-Agricultural" = "Other",
  "International Aviation" = "Transport",
  "International Shipping" = "Transport"
)

# =============================================================================
# CEDS PROVINCIAL MAPPINGS
# =============================================================================

# Raw sector codes to readable sector names
CEDS_PROVINCIAL_SECTOR_MAPPING <- c(
  "1A1a_Electricity-autoproducer" = "Electricity Autoproducer",
  "1A1a_Electricity-public" = "Electricity Public",
  "1A1a_Heat-production" = "Heat Production",
  "1A1bc_Other-transformation" = "Other Transformation",
  "1A2a_Ind-Comb-Iron-steel" = "Iron & Steel Combustion",
  "1A2b_Ind-Comb-Non-ferrous-metals" = "Non-Ferrous Metals Combustion",
  "1A2c_Ind-Comb-Chemicals" = "Chemicals Combustion",
  "1A2d_Ind-Comb-Pulp-paper" = "Pulp & Paper Combustion",
  "1A2e_Ind-Comb-Food-tobacco" = "Food & Tobacco Combustion",
  "1A2f_Ind-Comb-Non-metalic-minerals" = "Non-Metallic Minerals Combustion",
  "1A2g_Ind-Comb-Construction" = "Construction Combustion",
  "1A2g_Ind-Comb-machinery" = "Machinery Combustion",
  "1A2g_Ind-Comb-mining-quarying" = "Mining & Quarrying Combustion",
  "1A2g_Ind-Comb-other" = "Other Industry Combustion",
  "1A2g_Ind-Comb-textile-leather" = "Textile & Leather Combustion",
  "1A2g_Ind-Comb-transpequip" = "Transport Equipment Combustion",
  "1A2g_Ind-Comb-wood-products" = "Wood Products Combustion",
  "1A3aii_Domestic-aviation" = "Domestic Aviation",
  "1A3b_Road" = "Road Transport",
  "1A3c_Rail" = "Rail Transport",
  "1A3di_Oil_Tanker_Loading" = "Oil Tanker Loading",
  "1A3dii_Domestic-navigation" = "Domestic Navigation",
  "1A3eii_Other-transp" = "Other Transport",
  "1A4a_Commercial-institutional" = "Commercial & Institutional",
  "1A4b_Residential" = "Residential",
  "1A4c_Agriculture-forestry-fishing" = "Agriculture, Forestry & Fishing",
  "1A5_Other-unspecified" = "Other Unspecified",
  "1B1_Fugitive-solid-fuels" = "Fugitive Solid Fuels",
  "1B2_Fugitive-petr" = "Fugitive Petroleum",
  "1B2b_Fugitive-NG-distr" = "Fugitive Natural Gas Distribution",
  "1B2b_Fugitive-NG-prod" = "Fugitive Natural Gas Production",
  "1B2d_Fugitive-other-energy" = "Fugitive Other Energy",
  "2A1_Cement-production" = "Cement Production",
  "2A2_Lime-production" = "Lime Production",
  "2Ax_Other-minerals" = "Other Minerals",
  "2B_Chemical-industry" = "Chemical Industry",
  "2B2_Chemicals-Nitric-acid" = "Nitric Acid Production",
  "2B3_Chemicals-Adipic-acid" = "Adipic Acid Production",
  "2C1_Iron-steel-alloy-prod" = "Iron & Steel Production",
  "2C3_Aluminum-production" = "Aluminum Production",
  "2C4_Non-Ferrous-other-metals" = "Other Non-Ferrous Metals",
  "2D_Chemical-products-manufacture-processing" = "Chemical Products Processing",
  "2D_Degreasing-Cleaning" = "Degreasing & Cleaning",
  "2D_Other-product-use" = "Other Product Use",
  "2D_Paint-application" = "Paint Application",
  "2H_Pulp-and-paper-food-beverage-wood" = "Pulp, Paper, Food & Wood",
  "3B_Manure-management" = "Manure Management",
  "3D_Rice-Cultivation" = "Rice Cultivation",
  "3D_Soil-emissions" = "Soil Emissions",
  "3E_Enteric-fermentation" = "Enteric Fermentation",
  "3I_Agriculture-other" = "Other Agriculture",
  "5A_Solid-waste-disposal" = "Solid Waste Disposal",
  "5C_Waste-combustion" = "Waste Combustion",
  "5D_Wastewater-handling" = "Wastewater Handling",
  "5E_Other-waste-handling" = "Other Waste Handling",
  "6A_Other-in-total" = "Other In Total",
  "6B_Other-not-in-total" = "Other Not In Total",
  "7A_Fossil-fuel-fires" = "Fossil Fuel Fires",
  "7BC_Indirect-N2O-non-agricultural-N" = "Indirect N2O Non-Agricultural",
  "1A3ai_International-aviation" = "International Aviation",
  "1A3di_International-shipping" = "International Shipping"
)

# Readable sector names to sector groups (same as national)
CEDS_PROVINCIAL_SECTOR_GROUP_MAPPING <- CEDS_NATIONAL_SECTOR_GROUP_MAPPING

# =============================================================================
# CEDS MAP SECTOR MAPPING (for numeric sector IDs)
# =============================================================================

# Numeric sector IDs used by CEDS map class to readable sector names
CEDS_MAP_SECTOR_MAPPING <- c(
  "0" = "Agriculture",
  "1" = "Energy",
  "2" = "Industrial",
  "3" = "Transportation",
  "4" = "Residential, Commercial, Other",
  "5" = "Solvents production and application",
  "6" = "Waste",
  "7" = "International Shipping"
)

# Readable sector names from map class to sector groups
CEDS_MAP_SECTOR_GROUP_MAPPING <- c(
  "Agriculture" = "Agriculture",
  "Energy" = "Power Generation",
  "Industrial" = "Industry",
  "Transportation" = "Transport",
  "Residential, Commercial, Other" = "Buildings",
  "Solvents production and application" = "Industry",
  "Waste" = "Waste",
  "International Shipping" = "Transport"
)

#' @title Get Sector Name
#' @description Get the human-readable name for a sector code using the two-tier mapping system
#' @param sector_code The sector code
#' @param source The data source ("CEDS" or "EDGAR")
#' @param type The data type ("national" or "provincial")
#' @return The readable sector name or error if not found
#' @export
get_sector_name <- function(sector_code, source = "CEDS", type = "national") {
  # Special case: CEDS provincial with numeric sector IDs (used by map class)
  if (source == "CEDS" && type == "provincial" && grepl("^[0-9]+$", sector_code)) {
    if (!sector_code %in% names(CEDS_MAP_SECTOR_MAPPING)) {
      stop(glue::glue("Unknown numeric sector ID '{sector_code}' for CEDS provincial map data"))
    }
    return(unname(CEDS_MAP_SECTOR_MAPPING[sector_code]))
  }

  # Determine which mapping to use
  mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTOR_MAPPING")

  if (!exists(mapping_name)) {
    stop(glue::glue("Unknown source/type combination: {source}/{type}"))
  }

  mapping <- get(mapping_name)

  if (!sector_code %in% names(mapping)) {
    stop(glue::glue("Unknown sector code '{sector_code}' for {source} {type} data"))
  }

  return(unname(mapping[sector_code]))
}

#' @title Get Sector Group
#' @description Get the sector group for a sector code using the two-tier mapping system
#' @param sector_code The sector code
#' @param source The data source ("CEDS" or "EDGAR")
#' @param type The data type ("national" or "provincial")
#' @return The sector group or error if not found
#' @export
get_sector_group <- function(sector_code, source = "CEDS", type = "national") {
  # Special case: CEDS provincial with numeric sector IDs (used by map class)
  if (source == "CEDS" && type == "provincial" && grepl("^[0-9]+$", sector_code)) {
    if (!sector_code %in% names(CEDS_MAP_SECTOR_MAPPING)) {
      stop(glue::glue("Unknown numeric sector ID '{sector_code}' for CEDS provincial map data"))
    }
    sector_name <- unname(CEDS_MAP_SECTOR_MAPPING[sector_code])
    # Map the readable name to sector group using the map group mapping
    if (!sector_name %in% names(CEDS_MAP_SECTOR_GROUP_MAPPING)) {
      stop(glue::glue("Unknown sector name '{sector_name}' for CEDS provincial map data"))
    }
    return(unname(CEDS_MAP_SECTOR_GROUP_MAPPING[sector_name]))
  }

  # First get the readable sector name
  sector_name <- get_sector_name(sector_code, source, type)

  # Then get the sector group
  group_mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTOR_GROUP_MAPPING")

  if (!exists(group_mapping_name)) {
    stop(glue::glue("Unknown source/type combination: {source}/{type}"))
  }

  group_mapping <- get(group_mapping_name)

  if (!sector_name %in% names(group_mapping)) {
    stop(glue::glue("Unknown sector name '{sector_name}' for {source} {type} data"))
  }

  return(unname(group_mapping[sector_name]))
}

#' @title Validate Sector Code
#' @description Check if a sector code is valid for the given source and type
#' @param sector_code The sector code to validate
#' @param source The data source ("CEDS" or "EDGAR")
#' @param type The data type ("national" or "provincial")
#' @return TRUE if valid, FALSE otherwise
#' @export
validate_sector_code <- function(sector_code, source = "CEDS", type = "national") {
  tryCatch({
    get_sector_name(sector_code, source, type)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' @title Get Available Sectors
#' @description Get list of available sector codes for the given source and type
#' @param source The data source ("CEDS" or "EDGAR")
#' @param type The data type ("national" or "provincial")
#' @return Vector of available sector codes
#' @export
get_available_sectors <- function(source = "CEDS", type = "national") {
  mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTOR_MAPPING")

  if (!exists(mapping_name)) {
    stop(glue::glue("Unknown source/type combination: {source}/{type}"))
  }

  mapping <- get(mapping_name)
  return(names(mapping))
}

#' @title Get Sector Table
#' @description Get a data frame with all sectors and their names for the given source and type
#' @param source The data source ("CEDS" or "EDGAR")
#' @param type The data type ("national" or "provincial")
#' @return A tibble with sector codes, readable names, and sector groups
#' @export
get_sector_table <- function(source = "CEDS", type = "national") {
  sector_mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTOR_MAPPING")
  group_mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTOR_GROUP_MAPPING")

  if (!exists(sector_mapping_name) || !exists(group_mapping_name)) {
    stop(glue::glue("Unknown source/type combination: {source}/{type}"))
  }

  sector_mapping <- get(sector_mapping_name)
  group_mapping <- get(group_mapping_name)

  result <- tibble::tibble(
    sector_code = names(sector_mapping),
    sector_name = unname(sector_mapping),
    sector_group = unname(group_mapping[sector_mapping])
  )

  return(result[order(result$sector_code), ])
}
