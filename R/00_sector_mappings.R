#' @title Sector Mappings
#' @description Clean sector code to name mappings for each data source
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble

# CEDS Sector Mappings
CEDS_POLLUTANTS <- c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2")

#' @export
CEDS_NATIONAL_SECTORS <- c(
  "1A1a_Electricity-public" = "Energy",
  "1A1b_Industry" = "Energy",
  "1A1c_Other-transformation" = "Energy",
  "1A2_Manufacturing" = "Industrial",
  "1A3_Transport" = "Transportation",
  "1A4a_Commercial-institutional" = "Residential, Commercial, Other",
  "1A4b_Residential" = "Residential, Commercial, Other",
  "1A4c_Agriculture-forestry-fishing" = "Agriculture",
  "1A5_Other-fuel-combustion" = "Energy",
  "2A1_Cement-production" = "Industrial",
  "2A2_Lime-production" = "Industrial",
  "2A3_Glass-production" = "Industrial",
  "2A4_Other-minerals" = "Industrial",
  "2B1_Ammonia" = "Industrial",
  "2B2_Nitric-acid" = "Industrial",
  "2B3_Adipic-acid" = "Industrial",
  "2B4_Caprolactam" = "Industrial",
  "2B5_Carbide" = "Industrial",
  "2B6_Titanium-dioxide" = "Industrial",
  "2B7_Soda-ash" = "Industrial",
  "2B8_Petrochemicals" = "Industrial",
  "2B9_Other-chemicals" = "Industrial",
  "2C1_Iron-steel" = "Industrial",
  "2C2_Non-ferrous-metals" = "Industrial",
  "2C3_Chemicals" = "Industrial",
  "2C4_Pulp-paper" = "Industrial",
  "2C5_Food-beverages" = "Industrial",
  "2C6_Other-processes" = "Industrial",
  "2D1_Landfills" = "Waste",
  "2D2_Wastewater" = "Waste",
  "2D3_Waste-incineration" = "Waste",
  "2D4_Other-waste" = "Waste",
  "2E_Solvents" = "Solvents production and application",
  "2F_Product-uses" = "Industrial",
  "2G_Other-product-use" = "Industrial",
  "3A_Enteric-fermentation" = "Agriculture",
  "3B_Manure-management" = "Agriculture",
  "3C_Rice-cultivation" = "Agriculture",
  "3D_Agricultural-soils" = "Agriculture",
  "3E_Agricultural-waste-burning" = "Agriculture",
  "3F_Other-agriculture" = "Agriculture",
  "4A_Solid-waste-disposal" = "Waste",
  "4B_Biological-treatment-solid-waste" = "Waste",
  "4C_Wastewater-handling" = "Waste",
  "4D_Waste-incineration" = "Waste",
  "4E_Other-waste" = "Waste",
  "5A_International-aviation" = "Transportation",
  "5B_International-shipping" = "International Shipping"
)

#' @export
CEDS_PROVINCIAL_SECTORS <- c(
  "0" = "Agriculture",
  "1" = "Energy",
  "2" = "Industrial",
  "3" = "Transportation",
  "4" = "Residential, Commercial, Other",
  "5" = "Solvents production and application",
  "6" = "Waste",
  "7" = "International Shipping"
)

# EDGAR Sector Mappings
#' @export
EDGAR_NATIONAL_SECTORS <- c(
  "Energy" = "Energy",
  "Industry" = "Industry",
  "Transport" = "Transport",
  "Residential and other sectors" = "Residential and other sectors",
  "Agriculture" = "Agriculture",
  "Waste" = "Waste",
  "International shipping" = "International shipping",
  "International aviation" = "International aviation",
  "Total" = "Total"
)


# EDGAR_POLLUTANTS = c("BC", "CO", "NH3", "NMVOC", "NOx", "OC", "PM10", "PM25", "SO2")
EDGAR_POLLUTANTS = c("NOx", "SO2", "PM25")

#' @export
EDGAR_PROVINCIAL_SECTORS <- c(
  "ENE" = "Energy",
  "REF_TRF" = "Refineries and transformation",
  "IND" = "Industry",
  "TNR_Aviation_CDS" = "Transport - Aviation (CDS)",
  "TNR_Aviation_CRS" = "Transport - Aviation (CRS)",
  "TNR_Aviation_LTO" = "Transport - Aviation (LTO)",
  "TNR_Aviation_SPS" = "Transport - Aviation (SPS)",
  "TRO" = "Transport - Road",
  "TNR_Other" = "Transport - Other",
  "TNR_Ship" = "Transport - Ship",
  "RCO" = "Residential and Commercial",
  "PRO_FFF" = "Production - FFF",
  "NMM" = "Non-metallic minerals",
  "CHE" = "Chemical industry",
  "IRO" = "Iron and steel",
  "NFE" = "Non-ferrous metals",
  "NEU" = "Non-energy use",
  "PRU_SOL" = "Production - Solvents",
  "FOO_PAP" = "Food and paper",
  "MNM" = "Mining",
  "AWB" = "Agricultural waste burning",
  "AGS" = "Agriculture",
  "SWD_LDF" = "Waste - Landfills",
  "SWD_INC" = "Waste - Incineration",
  "WWT" = "Wastewater treatment",
  "TOTALS" = "Total Emissions"
)

#' @title Get Sector Name
#' @description Get the human-readable name for a sector code, with error handling
#' @param sector_code The sector code
#' @param source The data source ("CEDS" or "EDGAR")
#' @param type The data type ("national" or "provincial")
#' @return The sector name or error if not found
#' @export
get_sector_name <- function(sector_code, source = "CEDS", type = "national") {
  # Determine which mapping to use
  mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTORS")

  if (!exists(mapping_name)) {
    stop(glue::glue("Unknown source/type combination: {source}/{type}"))
  }

  mapping <- get(mapping_name)

  if (!sector_code %in% names(mapping)) {
    stop(glue::glue("Unknown sector code '{sector_code}' for {source} {type} data"))
  }

  return(unname(mapping[sector_code]))
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
  mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTORS")

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
#' @return A tibble with sector codes and names
#' @export
get_sector_table <- function(source = "CEDS", type = "national") {
  mapping_name <- paste0(toupper(source), "_", toupper(type), "_SECTORS")

  if (!exists(mapping_name)) {
    stop(glue::glue("Unknown source/type combination: {source}/{type}"))
  }

  mapping <- get(mapping_name)

  result <- tibble::tibble(
    sector_code = names(mapping),
    sector_name = unname(mapping)
  )

  return(result[order(result$sector_code), ])
}
