#' @title CEDS Sector Mappings
#' @description Clean sector code to name mappings for CEDS data source

# CEDS Pollutants
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
CEDS_PROVINCIAL_SECTOR_MAPPING <- CEDS_NATIONAL_SECTOR_MAPPING

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
