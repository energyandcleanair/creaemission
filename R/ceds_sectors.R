#' @title CEDS Sector Mappings and Utilities
#' @description Helper functions and mappings for CEDS sector codes and names
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble

# CEDS Sector Codes and Names (from netCDF files)
#' @export
CEDS_GRIDDED_SECTORS <- c(
  "0" = "Agriculture",
  "1" = "Energy", 
  "2" = "Industrial",
  "3" = "Transportation",
  "4" = "Residential, Commercial, Other",
  "5" = "Solvents production and application",
  "6" = "Waste",
  "7" = "International Shipping"
)

# CEDS Sector Codes with IPCC format (for validation and testing)
#' @export
CEDS_IPCC_SECTORS <- c(
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

# Special sectors that should be filtered out from sector-based visualizations
#' @export
CEDS_TOTAL_SECTORS <- c("TOTALS", "Total", "ALL_SECTORS")

# Mapping from CEDS gridded sector codes to display names
#' @export
CEDS_SECTOR_MAPPING <- list(
  # Map numeric codes to display names
  "0" = "Agriculture",
  "1" = "Energy",
  "2" = "Industrial", 
  "3" = "Transportation",
  "4" = "Residential, Commercial, Other",
  "5" = "Solvents production and application",
  "6" = "Waste",
  "7" = "International Shipping"
)

#' @title Get CEDS Gridded Sector Name
#' @description Get the human-readable name for a gridded sector code
#' @param sector_code The gridded sector code (e.g., "4")
#' @return The sector name or the original code if not found
#' @export
get_ceds_gridded_sector_name <- function(sector_code) {
  if (sector_code %in% names(CEDS_GRIDDED_SECTORS)) {
    return(unname(CEDS_GRIDDED_SECTORS[sector_code]))
  }
  return(sector_code)
}

#' @title Get CEDS IPCC Sector Category
#' @description Map an IPCC sector code to its corresponding CEDS category
#' @param sector_code The IPCC sector code (e.g., "1A4b_Residential")
#' @return The corresponding CEDS category or NULL if not found
#' @export
get_ceds_ipcc_sector_category <- function(sector_code) {
  if (sector_code %in% names(CEDS_IPCC_SECTORS)) {
    return(unname(CEDS_IPCC_SECTORS[sector_code]))
  }
  return(NULL)
}

#' @title Get All CEDS Gridded Sectors
#' @description Get a data frame with all gridded sectors and their mappings
#' @return A tibble with sector codes and names
#' @export
get_ceds_sector_table <- function() {
  result <- tibble::tibble(
    gridded_code = names(CEDS_GRIDDED_SECTORS),
    gridded_name = CEDS_GRIDDED_SECTORS
  )
  result <- result[order(result$gridded_code), ]
  return(result)
}

#' @title Print CEDS Sector Summary
#' @description Print a formatted summary of CEDS sectors and mappings
#' @export
print_ceds_sector_summary <- function() {
  cat("=== CEDS Sector Summary ===\n\n")
  
  cat("Gridded Sectors (", length(CEDS_GRIDDED_SECTORS), "):\n")
  for (code in names(CEDS_GRIDDED_SECTORS)) {
    cat(sprintf("  %-20s = %s\n", code, CEDS_GRIDDED_SECTORS[code]))
  }
  
  cat("\nIPCC Sectors (", length(CEDS_IPCC_SECTORS), "):\n")
  for (code in names(CEDS_IPCC_SECTORS)) {
    cat(sprintf("  %-30s -> %s\n", code, CEDS_IPCC_SECTORS[code]))
  }
}

#' @title Validate CEDS Sector
#' @description Check if a sector code is valid for CEDS data
#' @param sector_code The sector code to validate
#' @param type Either "gridded" or "ipcc"
#' @return TRUE if valid, FALSE otherwise
#' @export
validate_ceds_sector <- function(sector_code, type = "gridded") {
  if (type == "gridded") {
    return(sector_code %in% names(CEDS_GRIDDED_SECTORS))
  } else if (type == "ipcc") {
    return(sector_code %in% names(CEDS_IPCC_SECTORS))
  }
  return(FALSE)
}

#' @title Get Available CEDS Sectors
#' @description Get list of available sectors for a given type
#' @param type Either "gridded" or "ipcc"
#' @return Vector of available sector codes/names
#' @export
get_available_ceds_sectors <- function(type = "gridded") {
  if (type == "gridded") {
    return(names(CEDS_GRIDDED_SECTORS))
  } else if (type == "ipcc") {
    return(names(CEDS_IPCC_SECTORS))
  }
  return(character(0))
}

#' @title Check if Sector is a Total/Aggregated Sector
#' @description Check if a sector code represents a total or aggregated value
#' @param sector_code The sector code to check
#' @param source The data source ("EDGAR" or "CEDS")
#' @return TRUE if it's a total sector, FALSE otherwise
#' @export
is_total_sector <- function(sector_code, source = "EDGAR") {
  if (source == "EDGAR") {
    return(sector_code %in% EDGAR_TOTAL_SECTORS)
  } else if (source == "CEDS") {
    return(sector_code %in% CEDS_TOTAL_SECTORS)
  }
  return(FALSE)
}

#' @title Filter Out Total Sectors
#' @description Remove total/aggregated sectors from a dataset
#' @param data Data frame with sector column
#' @param sector_col Name of the sector column (default: "sector")
#' @param source The data source ("EDGAR" or "CEDS")
#' @return Filtered data frame without total sectors
#' @export
filter_out_total_sectors <- function(data, sector_col = "sector", source = "EDGAR") {
  if (!sector_col %in% names(data)) {
    warning(sprintf("Column '%s' not found in data", sector_col))
    return(data)
  }
  
  # Filter out total sectors
  filtered_data <- data[!is_total_sector(data[[sector_col]], source), ]
  
  removed_count <- nrow(data) - nrow(filtered_data)
  if (removed_count > 0) {
    message(sprintf("Removed %d rows with total sectors from %s data", removed_count, source))
  }
  
  return(filtered_data)
}

#' @title Get Sectors for Visualization
#' @description Get sectors suitable for sector-based visualizations (excluding totals)
#' @param data Data frame with sector information
#' @param sector_col Name of the sector column
#' @param source The data source ("EDGAR" or "CEDS")
#' @return Vector of sector codes/names suitable for visualization
#' @export
get_visualization_sectors <- function(data, sector_col = "sector", source = "EDGAR") {
  if (!sector_col %in% names(data)) {
    warning(sprintf("Column '%s' not found in data", sector_col))
    return(character(0))
  }
  
  all_sectors <- unique(data[[sector_col]])
  visualization_sectors <- all_sectors[!is_total_sector(all_sectors, source)]
  
  return(visualization_sectors)
}

#' @title Generic Sector Filtering for Applications
#' @description Filter data for sector-based visualizations, excluding totals
#' @param data Data frame with emissions data
#' @param sector_col Name of the sector column (default: "sector")
#' @param source The data source ("EDGAR" or "CEDS")
#' @param include_totals Whether to include total sectors (default: FALSE for sector-based viz)
#' @return Filtered data frame
#' @export
filter_sectors_for_viz <- function(data, sector_col = "sector", source = "EDGAR", include_totals = FALSE) {
  if (include_totals) {
    return(data)
  }
  
  return(filter_out_total_sectors(data, sector_col, source))
}

#' @title Get Sector Display Name
#' @description Get the display name for a sector, handling both EDGAR and CEDS
#' @param sector_code The sector code
#' @param source The data source ("EDGAR" or "CEDS")
#' @return The display name for the sector
#' @export
get_sector_display_name <- function(sector_code, source = "EDGAR") {
  if (source == "EDGAR") {
    return(get_edgar_gridded_sector_name(sector_code))
  } else if (source == "CEDS") {
    return(get_ceds_gridded_sector_name(sector_code))
  }
  return(sector_code)
}

#' @title Clean CEDS Sector Name
#' @description Clean CEDS sector names for display, preserving original codes for validation
#' @param sector_code The sector code to clean
#' @param preserve_code Whether to preserve the original code (default: TRUE for validation)
#' @return Cleaned sector name
#' @export
clean_ceds_sector_name <- function(sector_code, preserve_code = TRUE) {
  if (preserve_code) {
    # For validation purposes, return the original code
    return(sector_code)
  }
  
  # First try to get the display name
  display_name <- get_ceds_gridded_sector_name(sector_code)
  
  if (display_name != sector_code) {
    # If we found a mapping, return the display name
    return(display_name)
  }
  
  # If no mapping found, try IPCC format
  ipcc_category <- get_ceds_ipcc_sector_category(sector_code)
  if (!is.null(ipcc_category)) {
    return(ipcc_category)
  }
  
  # If still no mapping, apply general cleaning
  return(clean_sector_name(sector_code))
} 