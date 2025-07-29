build_all_emissions_data <- function(min_year = 2000,
                                     provincial_years = 2022,
                                     provincial_iso2s=c("ID","IN","CN", "TH", "VN", "ZA"),
                                     sources = c("CEDS", "EDGAR"),
                                     build_maps = TRUE) {
  sources <- toupper(sources)
  results <- list()

  # Build CEDS data if requested
  if ("CEDS" %in% sources) {
    message("Building CEDS emissions data...")
    ceds <- CEDSSource$new()
    ceds$build(min_year = min_year)
    results$CEDS <- ceds

    # Build provincial
    message("Building CEDS provincial emissions data...")
    ceds_provincial <- CEDSSourceProvincial$new()
    ceds_provincial$build(years = provincial_years, iso2s = provincial_iso2s)
    
    # Build maps if requested
    if (build_maps) {
      message("Building CEDS provincial maps...")
      ceds_provincial$build_maps(
        years = provincial_years,
        iso2s = provincial_iso2s
      )
    }
  }

  # Build EDGAR data if requested
  if ("EDGAR" %in% sources) {
    message("Building EDGAR emissions data...")
    edgar <- EDGARSource$new()
    edgar$build(min_year = min_year)
    results$EDGAR <- edgar

    # Build provincial
    message("Building EDGAR provincial emissions data...")
    edgar_provincial <- EDGARSourceProvincial$new()
    edgar_provincial$build(years = provincial_years, iso2s = provincial_iso2s)
    
    # Build maps if requested
    if (build_maps) {
      message("Building EDGAR provincial maps...")
      edgar_provincial$build_maps(
        years = provincial_years,
        iso2s = provincial_iso2s
      )
    }
  }

  message("All requested emissions data built successfully!")
  return(invisible(results))
}
