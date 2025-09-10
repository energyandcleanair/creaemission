#' @title EDGARMap
#' @description EDGAR-specific map source for handling NetCDF files
#'
#' ## Processing Pipeline Architecture:
#'
#' ### Raw Data Processing (Shared):
#' 1. `process_raw_edgar_files()` - Core processing logic for raw EDGAR files
#'    - Unit conversion (tonnes/cell/year → kg m-2 yr-1)
#'    - Sector identification and file grouping
#'    - Spatial processing and metadata setup
#'
#' ### Output Formats:
#' 2. `generate_netcdf_files()` - Creates compressed NetCDF files
#'    - Uses `process_raw_edgar_files()` for core processing
#'    - Applies shared post-processing (units validation)
#'    - Saves as NetCDF with compression
#'
#' 3. `generate_cog_files()` - Creates Cloud Optimized GeoTIFFs
#'    - Uses existing `get()` method to load processed data
#'    - Applies shared post-processing (leaflet extent cropping)
#'    - Saves as COG with tiling and overviews
#'
#' ### Shared Utilities:
#' - `post_process_raster()` - Common raster post-processing
#'   - Extent cropping (optional, for leaflet compatibility)
#'   - Units validation and standardization
#'
#' ## EDGAR vs CEDS Differences:
#' - **Input format**: EDGAR provides annual totals in tonnes/cell, CEDS provides monthly fluxes in kg m-2 s-1
#' - **Processing**: EDGAR converts area-based totals to flux rates, CEDS aggregates monthly data
#' - **File structure**: EDGAR has one file per pollutant/sector/year, CEDS has monthly layers per pollutant
#'
#' This architecture eliminates code duplication by separating:
#' - **Common processing logic** (unit conversion, spatial processing)
#' - **Format-specific output** (NetCDF vs COG)
#' - **Shared utilities** (post-processing, validation)
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
    #' @param formats Output formats: "netcdf", "cog", or both
    #' @param countries Vector of ISO3 country codes (for COGs)
    #' @return Invisibly returns list of processed files
    build = function(pollutants = names(EDGAR_POLLUTANTS),
                    sectors = names(EDGAR_PROVINCIAL_SECTOR_MAPPING),
                    years = NULL, formats = c("netcdf", "cog"), countries = c("wld")) {
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

      # Step 2: Generate maps in requested formats
      if (length(downloaded_files) > 0) {
        message(paste0("Generating maps in formats: ", paste(formats, collapse=", ")))
        processed_files <- self$generate_maps(downloaded_files, formats, countries)
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

      # Get all COG TIFF files
      tif_files <- list.files(self$data_dir, pattern = "_wld\\.tif$", full.names = TRUE)

      if (length(tif_files) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Parse filenames to extract metadata
      # Pattern: {pollutant}_{year}_{sector}_wld.tif
      available_data <- list()

      for (file in tif_files) {
        filename <- basename(file)
        # Remove _wld.tif suffix
        base_name <- gsub("_wld\\.tif$", "", filename)

        # Find the pollutant and year at the beginning
        # Pattern: pollutant_year_sectorname
        parts <- strsplit(base_name, "_")[[1]]

        if (length(parts) >= 3) {
          pollutant_from_file <- parts[1]
          # Map EDGAR pollutant codes to standard names
          pollutant_mapped <- map_values(pollutant_from_file, EDGAR_POLLUTANTS)
          year_from_file <- as.numeric(parts[2])

          # Everything after pollutant and year is the sector (may contain underscores/spaces)
          sector_from_file <- paste(parts[3:length(parts)], collapse = "_")

          if (!is.na(year_from_file) && !is.na(sector_from_file) && sector_from_file != "") {
            # Try to map sector - handles both codes and names
            sector_mapped <- map_values(sector_from_file, EDGAR_PROVINCIAL_SECTOR_MAPPING)

            # If mapping didn't work (returns original value), use as-is
            if (sector_mapped == sector_from_file && sector_from_file %in% EDGAR_PROVINCIAL_SECTOR_MAPPING) {
              # It's already a readable name
              sector_mapped <- sector_from_file
            }

            available_data[[length(available_data) + 1]] <- data.frame(
              pollutant = pollutant_mapped,
              sector = sector_mapped,
              year = year_from_file,
              stringsAsFactors = FALSE
            )
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

    #' @description Get raster using COG with sector code conversion
    #' @param pollutant Pollutant code
    #' @param sector Sector name (readable)
    #' @param year Year
    #' @param iso3 ISO3 country code
    #' @param prefer_cog Prefer COG over NetCDF if both exist
    #' @return Terra raster object or NULL if not available
    get_cog = function(pollutant, sector, year, iso3 = "wld", prefer_cog = TRUE) {
      # Convert readable sector name to sector code for filename lookup
      sector_code <- names(EDGAR_PROVINCIAL_SECTOR_MAPPING)[EDGAR_PROVINCIAL_SECTOR_MAPPING == sector]

      if (length(sector_code) == 0) {
        return(NULL)
      }

      # Call parent method with sector code
      return(super$get_cog(pollutant, sector_code, year, iso3, prefer_cog))
    },

    #' @description Get sector ID from sector name or code
    #' @param sector Sector name or code
    #' @return Sector ID or NULL if not found
    get_sector_id = function(sector) {
      # If sector is already a sector code
      if (sector %in% names(EDGAR_PROVINCIAL_SECTOR_MAPPING)) {
        return(sector)
      }

      # Try to find sector by name
      sector_code <- names(EDGAR_PROVINCIAL_SECTOR_MAPPING)[EDGAR_PROVINCIAL_SECTOR_MAPPING == sector]
      
      if (length(sector_code) > 0) {
        return(sector_code[1])  # Return first match if multiple
      }

      # If sector is not found, try to use it directly as a sector code
      return(sector)
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

    #' @description Generate map files in specified formats
    #' @param raw_files List of raw NetCDF file paths
    #' @param formats Vector of output formats: "netcdf", "cog", or both
    #' @param countries Vector of ISO3 country codes (for COGs)
    #' @param overwrite Whether to overwrite existing files
    #' @return Invisibly returns list of output files by format
    generate_maps = function(raw_files, formats = c("netcdf", "cog"), countries = c("wld"), overwrite = FALSE) {
      # Create data directory if it doesn't exist
      if (!dir.exists(self$data_dir)) {
        dir.create(self$data_dir, recursive = TRUE, showWarnings = FALSE)
      }

      output_files <- list()
      if ("netcdf" %in% formats) output_files$netcdf <- list()
      if ("cog" %in% formats) output_files$cog <- list()

      # SHARED PROCESSING: Group and process raw files
      processed_data_groups <- self$process_raw_edgar_files(raw_files)

      # Write in requested formats
      for (key in names(processed_data_groups)) {
        processed_data <- processed_data_groups[[key]]

        if (!is.null(processed_data)) {
          # Write NetCDF format if requested
          if ("netcdf" %in% formats) {
            netcdf_file <- self$write_netcdf_format(processed_data, key)
            if (!is.null(netcdf_file)) {
              output_files$netcdf[[length(output_files$netcdf) + 1]] <- netcdf_file
            }
          }

          # Write COG format if requested
          if ("cog" %in% formats) {
            cog_files <- self$write_cog_format(processed_data, key, countries, overwrite)
            if (length(cog_files) > 0) {
              output_files$cog <- c(output_files$cog, cog_files)
            }
          }
        }
      }

      return(invisible(output_files))
    },

    #' @description Process raw EDGAR files (shared processing logic)
    #' @param nc_files List of raw NetCDF file paths
    #' @return List of processed raster stacks grouped by pollutant_year
    process_raw_edgar_files = function(nc_files) {
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
      processed_data_groups <- list()
      for (key in names(file_groups)) {
        parts <- strsplit(key, "_")[[1]]
        pollutant <- parts[1]
        pollutant <- map_values(pollutant, EDGAR_POLLUTANTS)
        year <- parts[2]

        message(paste0("Processing ", pollutant, " for year ", year, " with ", length(file_groups[[key]]), " sectors"))

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

              # CRITICAL: Convert from t/cell/year to kg/m2/yr to be similar with CEDS (PRESERVE EXACT UNIT CONVERSION)
              area_m2 <- terra::cellSize(r_tonne_yr, unit="m")
              r_kg_m2_yr <- r_tonne_yr * 1000 / area_m2

              # CRITICAL: Set the units attribute (PRESERVE EXACT UNITS)
              terra::units(r_kg_m2_yr) <- "kg m-2 yr-1"

              # Set layer name to sector
              names(r_kg_m2_yr) <- sector

              sector_rasters[[sector]] <- r_kg_m2_yr

              message(paste0("Processed sector ", sector, " for ", pollutant, " ", year))
            } else {
              message(paste0("No emissions layer found in ", basename(nc_file)))
            }
          }, error = function(e) {
            message(paste0("Error processing ", basename(nc_file), ": ", e$message))
          })
        }

        # Combine all sectors into a single raster stack
        if (length(sector_rasters) > 0) {
          # Combine rasters using terra::rast() with names
          processed_stack <- terra::rast(sector_rasters)
          processed_data_groups[[key]] <- list(
            data = processed_stack,
            pollutant = pollutant,
            year = year
          )
        }
      }

      return(processed_data_groups)
    },

    #' @description Write processed data as NetCDF format
    #' @param processed_info List with data, pollutant, year
    #' @param key Pollutant_year key
    #' @return Path to written NetCDF file or NULL if failed
    write_netcdf_format = function(processed_info, key) {
      tryCatch({
        # Create processed filename: pollutant_year.nc
        processed_filename <- paste0(processed_info$pollutant, "_", processed_info$year, ".nc")
        dest_file <- file.path(self$data_dir, processed_filename)

        # Save the processed raster stack with units
        terra::writeCDF(processed_info$data, dest_file, overwrite = TRUE, split = TRUE, compress=9)
        message(paste0("Generated NetCDF: ", processed_filename))
        return(dest_file)
      }, error = function(e) {
        message(paste0("Error writing NetCDF for ", key, ": ", e$message))
        return(NULL)
      })
    },

    #' @description Write processed data as COG format
    #' @param processed_info List with data, pollutant, year
    #' @param key Pollutant_year key
    #' @param countries Vector of ISO3 country codes
    #' @param overwrite Whether to overwrite existing files
    #' @return Vector of created COG file paths
    write_cog_format = function(processed_info, key, countries, overwrite) {
      created_files <- character(0)

      pollutant <- processed_info$pollutant
      year <- processed_info$year
      processed_data <- processed_info$data

      # Generate COG for each sector and country combination
      sector_codes <- names(processed_data)

      for (sector_code in sector_codes) {
        sector_raster <- processed_data[[sector_code]]

        for (country in countries) {
          tryCatch({
            cog_path <- self$get_cog_path(pollutant, sector_code, year, country)

            # Skip if file exists and not overwriting
            if (file.exists(cog_path) && !overwrite) {
              created_files <- c(created_files, cog_path)
              next
            }

            # Apply country cropping if needed
            final_raster <- self$crop_to_country(sector_raster, country)

            # CRITICAL: Apply leaflet-friendly extent (avoid poles) for COG format
            final_raster <- terra::crop(final_raster, terra::ext(-180, 180, -89, 89))

            # Write as COG with optimizations (includes overviews automatically)
            terra::writeRaster(
              final_raster,
              cog_path,
              overwrite = TRUE,
              filetype = "COG",
              gdal = c("COMPRESS=LZW", "OVERVIEW_RESAMPLING=BILINEAR")
            )

            created_files <- c(created_files, cog_path)

          }, error = function(e) {
            message(paste0("Error creating COG for ", pollutant, "_", sector_name, "_", year, "_", country, ": ", e$message))
          })
        }
      }

      if (length(created_files) > 0) {
        message(paste0("Generated ", length(created_files), " COG files from ", key))
      }

      return(created_files)
    }
  )
)

