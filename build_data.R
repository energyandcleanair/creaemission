build_all_emissions_data <- function(min_year = 2000, 
                                    sources = c("CEDS", "EDGAR")) {
  sources <- toupper(sources)
  results <- list()
  
  # Build CEDS data if requested
  if ("CEDS" %in% sources) {
    message("Building CEDS emissions data...")
    ceds <- CEDSSource$new()
    ceds$build(min_year = min_year)
    results$CEDS <- ceds
  }
  
  # Build EDGAR data if requested
  if ("EDGAR" %in% sources) {
    message("Building EDGAR emissions data...")
    edgar <- EDGARSource$new()
    edgar$build(min_year = min_year)
    results$EDGAR <- edgar
  }
  
  message("All requested emissions data built successfully!")
  return(invisible(results))
}