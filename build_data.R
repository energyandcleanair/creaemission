build_data <- function(min_year = 2000,
                       provincial_years = seq(2000, 2022),
                       provincial_iso2s=c("ID", "IN", "ZA", "TH", "CN"),
                       sources = c("CEDS", "EDGAR"),
                       build_maps = TRUE) {
  sources <- toupper(sources)
  results <- list()

  library(tidyverse)

  # Build CEDS data if requested
  if ("CEDS" %in% sources) {
    message("Building CEDS emissions data...")
    ceds <- CEDSNational$new()
    ceds$build(min_year = min_year)
    results$CEDS <- ceds

    # Build provincial
    message("Building CEDS provincial emissions data...")
    ceds_provincial <- CEDSProvincial$new()
    ceds_provincial$build(iso2s = provincial_iso2s, years = provincial_years)

    # Build maps if requested
    if (build_maps) {
      message("Building CEDS map data...")
      ceds_provincial$map_source$build(years = provincial_years)
    }
  }

  # Build EDGAR data if requested
  if ("EDGAR" %in% sources) {
    message("Building EDGAR emissions data...")
    edgar <- EDGARNational$new()
    edgar$build(min_year = min_year)
    results$EDGAR <- edgar

    # Build provincial
    message("Building EDGAR provincial emissions data...")
    edgar_provincial <- EDGARProvincial$new()
    edgar_provincial$build(iso2s = provincial_iso2s, years = provincial_years)

    # Build maps if requested
    if (build_maps) {
      message("Building EDGAR map data...")
      edgar_provincial$map_source$build(years = provincial_years)
    }
  }

  message("All requested emissions data built successfully!")
  return(invisible(results))
}
