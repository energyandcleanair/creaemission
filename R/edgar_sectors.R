#' @title EDGAR Sector Mappings and Utilities
#' @description Helper functions and mappings for EDGAR sector codes and names
#'
#' @importFrom dplyr %>%
#' @importFrom tibble tibble

# EDGAR Gridded Sectors (from netCDF files)
#' @export
EDGAR_GRIDDED_SECTORS <- c(
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

# Special sectors that should be filtered out from sector-based visualizations
#' @export
EDGAR_TOTAL_SECTORS <- c("TOTALS")

# EDGAR National Sectors (from precomputed datasets)
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

# Mapping from gridded sectors to national sectors
#' @export
EDGAR_SECTOR_MAPPING <- list(
  # Energy
  "ENE" = "Energy",
  "REF_TRF" = "Energy",
  
  # Industry
  "IND" = "Industry",
  "PRO_FFF" = "Industry", 
  "NMM" = "Industry",
  "CHE" = "Industry",
  "IRO" = "Industry",
  "NFE" = "Industry",
  "NEU" = "Industry",
  "PRU_SOL" = "Industry",
  "FOO_PAP" = "Industry",
  "MNM" = "Industry",
  
  # Transport
  "TNR_Aviation_CDS" = "Transport",
  "TNR_Aviation_CRS" = "Transport",
  "TNR_Aviation_LTO" = "Transport", 
  "TNR_Aviation_SPS" = "Transport",
  "TRO" = "Transport",
  "TNR_Other" = "Transport",
  "TNR_Ship" = "Transport",
  
  # Residential and other sectors
  "RCO" = "Residential and other sectors",
  
  # Agriculture
  "AWB" = "Agriculture",
  "AGS" = "Agriculture",
  
  # Waste
  "SWD_LDF" = "Waste",
  "SWD_INC" = "Waste", 
  "WWT" = "Waste",
  
  # Total
  "TOTALS" = "Total"
)

#' @title Get EDGAR Gridded Sector Name
#' @description Get the human-readable name for a gridded sector code
#' @param sector_code The gridded sector code (e.g., "RCO")
#' @return The sector name or the original code if not found
#' @export
get_edgar_gridded_sector_name <- function(sector_code) {
  if (sector_code %in% names(EDGAR_GRIDDED_SECTORS)) {
    return(EDGAR_GRIDDED_SECTORS[sector_code])
  }
  return(sector_code)
}

#' @title Get EDGAR National Sector
#' @description Map a gridded sector code to its corresponding national sector
#' @param sector_code The gridded sector code (e.g., "RCO")
#' @return The corresponding national sector name or NULL if not found
#' @export
get_edgar_national_sector <- function(sector_code) {
  if (sector_code %in% names(EDGAR_SECTOR_MAPPING)) {
    return(EDGAR_SECTOR_MAPPING[[sector_code]])
  }
  return(NULL)
}

#' @title Get All EDGAR Gridded Sectors
#' @description Get a data frame with all gridded sectors and their mappings
#' @return A tibble with sector codes, names, and national mappings
#' @export
get_edgar_sector_table <- function() {
  result <- tibble::tibble(
    gridded_code = names(EDGAR_GRIDDED_SECTORS),
    gridded_name = EDGAR_GRIDDED_SECTORS,
    national_sector = sapply(names(EDGAR_GRIDDED_SECTORS), get_edgar_national_sector)
  )
  result <- result[order(result$national_sector, result$gridded_code), ]
  return(result)
}

#' @title Print EDGAR Sector Summary
#' @description Print a formatted summary of EDGAR sectors and mappings
#' @export
print_edgar_sector_summary <- function() {
  cat("=== EDGAR Sector Summary ===\n\n")
  
  cat("Gridded Sectors (", length(EDGAR_GRIDDED_SECTORS), "):\n")
  for (code in names(EDGAR_GRIDDED_SECTORS)) {
    cat(sprintf("  %-20s = %s\n", code, EDGAR_GRIDDED_SECTORS[code]))
  }
  
  cat("\nNational Sectors (", length(EDGAR_NATIONAL_SECTORS), "):\n")
  for (name in names(EDGAR_NATIONAL_SECTORS)) {
    cat(sprintf("  %s\n", name))
  }
  
  cat("\nSector Mappings:\n")
  sector_table <- get_edgar_sector_table()
  for (national in unique(sector_table$national_sector)) {
    if (!is.null(national)) {
      sectors <- sector_table[sector_table$national_sector == national, "gridded_code"]
      cat(sprintf("  %-25s <- %s\n", national, paste(sectors, collapse = ", ")))
    }
  }
}

#' @title Validate EDGAR Sector
#' @description Check if a sector code is valid for EDGAR data
#' @param sector_code The sector code to validate
#' @param type Either "gridded" or "national"
#' @return TRUE if valid, FALSE otherwise
#' @export
validate_edgar_sector <- function(sector_code, type = "gridded") {
  if (type == "gridded") {
    return(sector_code %in% names(EDGAR_GRIDDED_SECTORS))
  } else if (type == "national") {
    return(sector_code %in% names(EDGAR_NATIONAL_SECTORS))
  }
  return(FALSE)
}

#' @title Get Available EDGAR Sectors
#' @description Get list of available sectors for a given type
#' @param type Either "gridded" or "national"
#' @return Vector of available sector codes/names
#' @export
get_available_edgar_sectors <- function(type = "gridded") {
  if (type == "gridded") {
    return(names(EDGAR_GRIDDED_SECTORS))
  } else if (type == "national") {
    return(names(EDGAR_NATIONAL_SECTORS))
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
    # Add CEDS total sectors when known
    ceds_total_sectors <- c("TOTALS", "Total", "ALL_SECTORS")
    return(sector_code %in% ceds_total_sectors)
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
    # Add CEDS sector name mapping when available
    # For now, return the original code
    return(sector_code)
  }
  return(sector_code)
} 