# CEDS gridded data extends to 2023; EDGAR v8.1 air pollutants to 2022 — use separate year arguments per source.
build_data <- function(min_year = 2000,
                       ceds_provincial_years = seq(2000, 2023),
                       edgar_provincial_years = seq(2000, 2022),
                       ceds_map_years = 2023,
                       edgar_map_years = 2022,
                       provincial_iso2s=c("ID", "IN", "ZA", "TH", "CN"),
                       sources = c("CEDS", "EDGAR"),
                       build_national = TRUE,
                       build_maps = TRUE,
                       build_provincial = TRUE,
                       keep_raw_cache = FALSE) {
  sources <- toupper(sources)
  results <- list()

  library(tidyverse)

  # Build CEDS data if requested
  if ("CEDS" %in% sources) {
    if (build_national) {
      message("Building CEDS emissions data...")
      ceds <- CEDSNational$new()
      ceds$build(min_year = min_year)
      results$CEDS <- ceds
    }

    # Build provincial if requested
    if (build_provincial) {
      message("Building CEDS provincial emissions data...")
      ceds_provincial <- CEDSProvincial$new()
      ceds_provincial$build(iso2s = provincial_iso2s, years = ceds_provincial_years)

      # Build maps if requested
      if (build_maps) {
        message("Building CEDS map data...")
        ceds_provincial$map_source$build(
          years = ceds_map_years,
          formats = c("cog"),
          keep_raw_cache = keep_raw_cache
        )
      }
    }
  }

  # Build EDGAR data if requested
  if ("EDGAR" %in% sources) {
    if (build_national) {
      message("Building EDGAR emissions data...")
      edgar <- EDGARNational$new()
      edgar$build(min_year = min_year, keep_raw_cache = keep_raw_cache)
      results$EDGAR <- edgar
    }

    # Build provincial if requested
    if (build_provincial) {
      message("Building EDGAR provincial emissions data...")
      edgar_provincial <- EDGARProvincial$new()
      edgar_provincial$build(iso2s = provincial_iso2s, years = edgar_provincial_years)

      # Build maps if requested
      if (build_maps) {
        message("Building EDGAR map data...")
        edgar_provincial$map_source$build(
          years = edgar_map_years,
          formats = c("cog"),
          keep_raw_cache = keep_raw_cache
        )
      }
    }
  }

  message("All requested emissions data built successfully!")
  return(invisible(results))
}
