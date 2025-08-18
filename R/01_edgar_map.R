#' @title EDGARMap
#' @description EDGAR-specific map source for handling NetCDF files
#'
#' @importFrom R6 R6Class
#' @export
EDGARMap <- R6::R6Class(
  "EDGARMap",
  inherit = SourceMap,

  public = list(
    #' @field version Data version
    version = NULL,

    #' @field available_years Available years
    available_years = NULL,

    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @description Initialize EDGAR map source
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Data directory path
    initialize = function(version = "v8.1",
                          available_years = 2000:2022,
                          data_dir = NULL) {
      # Use path resolution if data_dir is not provided
      if (is.null(data_dir)) {
        data_dir <- get_data_path(c("edgar", "maps"))
      }

      super$initialize(data_dir = data_dir)
      self$version <- version
      self$available_years <- available_years
      self$cache_dir <- get_cache_folder("edgar")

      # Create directories if they don't exist
      for (dir in c(self$data_dir, self$cache_dir)) {
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
    },

    #' @description Build EDGAR map data
    #' @param pollutants Pollutants to build (default: all available)
    #' @param sectors Sectors to build (default: all available)
    #' @param years Years to build (default: all available)
    #' @return Invisibly returns list of processed files
    build = function(pollutants =names(EDGAR_POLLUTANTS),
                    sectors = names(EDGAR_PROVINCIAL_SECTOR_MAPPING),
                    years = NULL) {
      message("Building EDGAR map data...")

      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Step 1: Download raw NetCDF files
      downloaded_files <- list()
      for (poll in pollutants) {
        for (sector in sectors) {
          for (year in years) {
            message(glue::glue("Downloading EDGAR map data for {poll} {sector} in {year}"))
            nc_files <- self$download_nc(poll, sector)
            if (length(nc_files) > 0) {
              # Filter files by year
              filtered_files <- self$filter_files_by_year(nc_files, year)
              if (length(filtered_files) > 0) {
                downloaded_files <- c(downloaded_files, filtered_files)
              }
            }
          }
        }
      }

      # Step 2: Process downloaded files (convert units, etc.)
      if (length(downloaded_files) > 0) {
        message("Processing downloaded NetCDF files...")
        processed_files <- self$process_netcdf_files(downloaded_files)
        message("EDGAR map data build complete!")
        return(invisible(processed_files))
      } else {
        message("No files downloaded for processing")
        return(invisible(character(0)))
      }
    },

    #' @description List available data combinations
    #' @param year Optional year filter
    #' @param sector Optional sector filter
    #' @param pollutant Optional pollutant filter
    #' @return Data frame with available pollutant/sector/year combinations
    list_available_data = function(year = NULL, sector = NULL, pollutant = NULL) {
      if (!dir.exists(self$data_dir)) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Get all processed NetCDF files
      nc_files <- list.files(self$data_dir, pattern = "\\.nc$", full.names = TRUE)

      if (length(nc_files) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Parse filenames to extract metadata
      available_data <- list()

      for (file in nc_files) {
        filename <- basename(file)
        # Parse EDGAR processed filename pattern: pollutant_year_vversion.nc
        parts <- strsplit(tools::file_path_sans_ext(filename), "_")[[1]]

        if (length(parts) >= 2) {
          # Extract pollutant (parts[1]), year (parts[2])
          pollutant_from_file <- parts[1]
          pollutant_mapped <- map_values(pollutant_from_file, EDGAR_POLLUTANTS)
          year_from_file <- as.numeric(parts[2])

          if (!is.na(year_from_file)) {
            # Processed files have multiple layers (one per sector)
            tryCatch({
              nc_stack <- terra::rast(file)
              sector_names <- names(nc_stack)

              if (length(sector_names) > 0) {
                for (sector_name in sector_names) {
                  available_data[[length(available_data) + 1]] <- data.frame(
                    pollutant = pollutant_mapped,
                    sector = map_values(sector_name, EDGAR_PROVINCIAL_SECTOR_MAPPING),
                    year = year_from_file,
                    stringsAsFactors = FALSE
                  )
                }
              }
            }, error = function(e) {
              # Skip files that can't be read
              warning(glue::glue("Could not read processed NetCDF file {file}: {e$message}"))
            })
          }
        }
      }

      if (length(available_data) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Combine data frames more robustly
      if (length(available_data) > 0) {
        result <- do.call(rbind, available_data)
        # Reset row names to avoid conflicts
        rownames(result) <- NULL
      } else {
        result <- data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          stringsAsFactors = FALSE
        )
      }

      # Apply filters if provided
      if (!is.null(pollutant)) {
        result <- result[result$pollutant %in% pollutant, ]
      }

      if (!is.null(year)) {
        result <- result[result$year %in% year, ]
      }

      if (!is.null(sector)) {
        result <- result[result$sector %in% sector, ]
      }

      return(result)
    },

    #' @description Get emissions raster
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso3 ISO3 country code
    #' @return Terra raster object or NULL if not available
    get = function(pollutant, sector, year, iso3) {

      # Look for processed NetCDF file with new naming pattern
      # Pattern: pollutant_year_vversion.nc
      nc_file <- file.path(self$data_dir, paste0(pollutant, "_", year, ".nc"))

      if (!file.exists(nc_file)) {
        return(NULL)
      }

      # Load the processed NetCDF file
      nc_stack <- terra::rast(nc_file)

      # Get the sector layer (processed files now have sector layers)
      # Convert sector code back to sector name for NetCDF lookup
      sector_id <- map_values(sector, setNames(names(EDGAR_PROVINCIAL_SECTOR_MAPPING), unname(EDGAR_PROVINCIAL_SECTOR_MAPPING)))

      if (sector_id %in% names(nc_stack)) {
        sector_raster <- nc_stack[[sector_id]]
      } else {
        return(NULL)
      }

      # Crop to country if specified
      if (iso3 != "wld") {
        iso2 <- countrycode::countrycode(iso3, "iso3c", "iso2c")
        country_boundaries <- terra::vect(creahelpers::get_adm(level = 0, res = "low", iso2s = iso2))

        if (!is.null(country_boundaries)) {
          sector_raster <- terra::crop(sector_raster, country_boundaries)
          sector_raster <- terra::mask(sector_raster, country_boundaries)
        }
      }

      return(sector_raster)
    },

    #' @description Clear all built data
    #' @return Invisibly returns the number of files removed
    clear = function() {
      removed_count <- 0

      if (dir.exists(self$data_dir)) {
        nc_files <- list.files(self$data_dir, pattern = "\\.nc$", full.names = TRUE)
        for (file in nc_files) {
          if (file.remove(file)) {
            removed_count <- removed_count + 1
          }
        }
      }

      message(glue::glue("Cleared {removed_count} EDGAR map files"))
      return(invisible(removed_count))
    },

    #' @description Download NetCDF file to cache
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @return Path to downloaded file or NULL if download failed
    download_nc = function(pollutant, sector) {
      # Create cache directory
      cache_dir <- file.path(self$cache_dir, "gridded")
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      }

      url <- glue::glue("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v81_FT2022_AP_new/{pollutant}/{sector}/{sector}_emi_nc.zip")

      dest_file_zip <- glue::glue("{cache_dir}/{pollutant}_{sector}_v{self$version}.zip")

      # Check if we already have extracted .nc files for this pollutant/sector
      existing_files <- list.files(cache_dir, pattern = glue::glue("{pollutant}.*{sector}.*\\.nc$"), full.names = TRUE)
      if (length(existing_files) > 0) {
        message(glue::glue("Files already exist for {pollutant} {sector}"))
        return(existing_files)
      }

      # Check if zip file exists and is valid
      if (file.exists(dest_file_zip) && file.info(dest_file_zip)$size > 1e6) {
        message(glue::glue("Zip file exists for {pollutant} {sector}, extracting..."))
        tryCatch({
          unzip(dest_file_zip, exdir = cache_dir)
          nc_files <- list.files(cache_dir, pattern = glue::glue("{pollutant}.*{sector}.*\\.nc$"), full.names = TRUE)
          if (length(nc_files) > 0) {
            message(glue::glue("Successfully extracted {length(nc_files)} files for {pollutant} {sector}"))
            return(nc_files)
          }
        }, error = function(e) {
          message(glue::glue("Error extracting existing zip for {pollutant} {sector}: {e$message}"))
        })
      }

      # Download if needed
      options(timeout = 300)
      options(download.file.method = "libcurl", url.method = "libcurl")

      tryCatch({
        download.file(url, dest_file_zip, mode = "wb")
        unzip(dest_file_zip, exdir = cache_dir)

        # Find all .nc files for this pollutant/sector
        nc_files <- list.files(cache_dir, pattern = glue::glue("{pollutant}.*{sector}.*\\.nc$"), full.names = TRUE)

        if (length(nc_files) > 0) {
          message(glue::glue("Successfully downloaded and extracted {length(nc_files)} files for {pollutant} {sector}"))
          return(nc_files)
        } else {
          message(glue::glue("Downloaded but no .nc files found for {pollutant} {sector}"))
          return(character(0))
        }
      }, error = function(e) {
        message(glue::glue("Error downloading {url}: {e$message}"))
        return(character(0))
      })
    },

    #' @description Filter files by year
    #' @param nc_files List of NetCDF file paths
    #' @param years Years to filter for
    #' @return Filtered list of file paths
    filter_files_by_year = function(nc_files, years) {
      # Extract year from filenames and filter
      filtered_files <- character(0)

      for (file in nc_files) {
        filename <- basename(file)
        # Parse filename like: v8.1_FT2022_AP_NMVOC_2022_ENE_emi.nc
        parts <- strsplit(filename, "_")[[1]]
        if (length(parts) >= 5) {
          file_year <- as.numeric(parts[5])
          if (file_year %in% years) {
            filtered_files <- c(filtered_files, file)
          }
        }
      }

      message(glue::glue("Filtered to {length(filtered_files)} files for years {paste(years, collapse=', ')}"))
      return(filtered_files)
    },

    #' @description Process downloaded NetCDF files
    #' @param nc_files List of NetCDF file paths
    #' @return Invisibly returns processed file paths
    process_netcdf_files = function(nc_files) {
      # Create data directory if it doesn't exist
      if (!dir.exists(self$data_dir)) {
        dir.create(self$data_dir, recursive = TRUE, showWarnings = FALSE)
      }

      processed_files <- list()

      # Group files by pollutant and year
      file_groups <- list()
      for (nc_file in nc_files) {
        if (file.exists(nc_file)) {
          filename <- basename(nc_file)
          # Parse EDGAR filename pattern: v8.1_FT2022_AP_NMVOC_2022_RCO_emi.nc
          # or v8.1_FT2022_AP_PM2.5_2022_SWD_INC_emi.nc
          parts <- strsplit(filename, "_")[[1]]
          if (length(parts) >= 6) {
            pollutant <- parts[4]
            year <- parts[5]

            # Handle sectors with underscores (like SWD_INC)
            # Find the sector part by looking for the pattern before "_emi.nc"
            sector_part <- gsub("_emi\\.nc$", "", filename)
            sector_part <- gsub(paste0("^.*_", pollutant, "_", year, "_"), "", sector_part)
            sector <- sector_part

            key <- paste(pollutant, year, sep = "_")
            if (!(key %in% names(file_groups))) {
              file_groups[[key]] <- list()
            }
            file_groups[[key]][[sector]] <- nc_file
          }
        }
      }

      # Process each pollutant-year combination
      for (key in names(file_groups)) {
        parts <- strsplit(key, "_")[[1]]
        pollutant <- parts[1]
        pollutant <- map_values(pollutant, EDGAR_POLLUTANTS)
        year <- parts[2]

        message(glue::glue("Processing {pollutant} for year {year} with {length(file_groups[[key]])} sectors"))

        sector_rasters <- list()

        for (sector in names(file_groups[[key]])) {
          nc_file <- file_groups[[key]][[sector]]

          tryCatch({
            # Load the raw NetCDF file
            nc_stack <- terra::rast(nc_file)
            stopifnot(all(terra::units(nc_stack)=="Tonnes"))

            # EDGAR files are in tonnes (total annual emissions per cell)
            # First, get the emissions layer
            if ("emissions" %in% names(nc_stack)) {
              r_tonne_yr <- nc_stack[["emissions"]]

              # Convert from t/cell/year to kg/m2/s to be similar with CEDS
              area_m2 <- terra::cellSize(r_tonne_yr, unit="m")
              r_kg_m2_yr <- r_tonne_yr * 1000 / area_m2

              # Set the units attribute
              terra::units(r_kg_m2_yr) <- "kg m-2 yr-1"

              # Set layer name to sector
              names(r_kg_m2_yr) <- sector

              sector_rasters[[sector]] <- r_kg_m2_yr

              message(glue::glue("Processed sector {sector} for {pollutant} {year}"))
            } else {
              message(glue::glue("No emissions layer found in {basename(nc_file)}"))
            }
          }, error = function(e) {
            message(glue::glue("Error processing {basename(nc_file)}: {e$message}"))
          })
        }

        # Combine all sectors into a single raster stack
        if (length(sector_rasters) > 0) {
          # Combine rasters using terra::rast() with names
          processed_stack <- terra::rast(sector_rasters)

          # Create processed filename: pollutant_year_vversion.nc
          processed_filename <- paste0(pollutant, "_", year, ".nc")
          dest_file <- file.path(self$data_dir, processed_filename)

          # Save the processed raster stack with units
          terra::writeCDF(processed_stack, dest_file, overwrite = TRUE, split = TRUE, compress=9)
          processed_files[[length(processed_files) + 1]] <- dest_file

          message(glue::glue("Processed and saved combined file: {processed_filename}"))
        }
      }

      return(invisible(processed_files))
    }
  )
)
