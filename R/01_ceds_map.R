#' @title CEDSMap
#' @description CEDS-specific map source for handling NetCDF files
#'
#' @importFrom R6 R6Class
#' @export
CEDSMap <- R6::R6Class(
  "CEDSMap",
  inherit = SourceMap,

  public = list(
    #' @field version Data version
    version = NULL,

    #' @field available_years Available years
    available_years = NULL,

    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @description Initialize CEDS map source
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Data directory path
    initialize = function(version = "2024_11_25",
                          available_years = 2000:2022,
                          data_dir = NULL) {
      # Use path resolution if data_dir is not provided
      if (is.null(data_dir)) {
        data_dir <- get_data_path(c("ceds", "maps"))
      }

      super$initialize(data_dir = data_dir)
      self$version <- version
      self$available_years <- available_years
      self$cache_dir <- get_cache_folder("ceds")

      # Create directories if they don't exist
      for (dir in c(self$data_dir, self$cache_dir)) {
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
    },

    #' @description Build map data (download raw files and process them)
    #' @param pollutants Vector of pollutants to download
    #' @param years Years to download
    #' @return Invisibly returns paths to saved files
    build = function(pollutants = c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2"),
                     years = NULL) {
      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Download raw files to cache
      downloaded_files <- list()
      for (poll in pollutants) {
        for (year in years) {
          message(glue::glue("Downloading CEDS map data for {poll} in {year}"))
          nc_file <- self$download_nc(poll, year)
          if (!is.null(nc_file)) {
            downloaded_files[[length(downloaded_files) + 1]] <- nc_file
          }
        }
      }

      # Process downloaded files
      if (length(downloaded_files) > 0) {
        message("Processing downloaded NetCDF files...")
        self$process_netcdf_files(downloaded_files)
      }

      message("CEDS map data build complete!")
      return(invisible(downloaded_files))
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

      # Get all NetCDF files
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
        parts <- strsplit(filename, "_")[[1]]

        if (length(parts) >= 2) {
          pollutant_from_file <- parts[1]
          year_from_file <- as.numeric(gsub("\\.nc$", "", parts[2]))

          if (!is.na(year_from_file)) {
            # Get available sectors from the processed NetCDF file
            tryCatch({
              nc_stack <- terra::rast(file)
              sector_names <- names(nc_stack)

              available_data[[length(available_data) + 1]] <- data.frame(
                pollutant = pollutant_from_file,
                sector = sector_names,
                year = year_from_file,
                stringsAsFactors = FALSE
              )

            }, error = function(e) {
              # Skip files that can't be read
              warning(glue::glue("Could not read NetCDF file {file}: {e$message}"))
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

      result <- do.call(rbind, available_data)

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
      # Look for processed NetCDF file
      nc_file <- file.path(self$data_dir, paste0(pollutant, "_", year, "_v", self$version, ".nc"))

      if (!file.exists(nc_file)) {
        return(NULL)
      }

      # Load the processed NetCDF file
      nc_stack <- terra::rast(nc_file)

      # Get the sector layer (processed files now have sector layers instead of monthly layers)
      if (sector %in% names(nc_stack)) {
        sector_raster <- nc_stack[[sector]]
      } else {
        return(NULL)
      }

      # Crop to country if specified (using parent class method)
      sector_raster <- self$crop_to_country(sector_raster, iso3)

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

      message(glue::glue("Cleared {removed_count} CEDS map files"))
      return(invisible(removed_count))
    },

    #' @description Download NetCDF file to cache
    #' @param pollutant Pollutant code
    #' @param year Year
    #' @return Path to downloaded file or NULL if download failed
    download_nc = function(pollutant, year) {
      # Create cache directory
      cache_dir <- file.path(self$cache_dir, "gridded")
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Construct URL based on pollutant and year
      # https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_11_25/gridded_emissions/bulk_emissions/fine_grids/SO2/SO2-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-11-25_gn_202201-202212.nc
      url <- glue("https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_{gsub('-','_',self$version)}/gridded_emissions/bulk_emissions/fine_grids/{pollutant}/{pollutant}-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-{gsub('_','-',self$version)}_gn_{year}01-{year}12.nc")

      dest_file <- file.path(cache_dir, glue::glue("{pollutant}_{year}_v{self$version}.nc"))

      # Skip if file already exists and is large enough
      if (file.exists(dest_file) && file.info(dest_file)$size > 1.6e8) {
        message(glue::glue("File already exists in cache: {dest_file}"))
        return(dest_file)
      }

      # Download with timeout settings
      options(timeout = 300)
      options(download.file.method = "libcurl", url.method = "libcurl")
      download.file(url, dest_file)
      return(dest_file)
    },

    #' @description Process downloaded NetCDF files
    #' @param nc_files List of NetCDF file paths from cache
    #' @return Invisibly returns processed file paths
    process_netcdf_files = function(nc_files) {
      # Create data directory if it doesn't exist
      if (!dir.exists(self$data_dir)) {
        dir.create(self$data_dir, recursive = TRUE, showWarnings = FALSE)
      }

      processed_files <- list()

      for (nc_file in nc_files) {
        if (file.exists(nc_file)) {
          message(glue::glue("Processing: {basename(nc_file)}"))

          # Load the raw NetCDF file
          r_kg_m2_s <- terra::rast(nc_file)
          stopifnot(unique(terra::units(r_kg_m2_s)) == "kg m-2 s-1")

          # Convert to kg m-2 yr
          r_kg_m2_yr <- r_kg_m2_s * 365 * 24 * 3600

          # CEDS files contain monthly data (12 months) for each sector
          # Layer names are like "SO2_em_anthro_sector=0_1" where:
          # - sector=0 means sector 0
          # - _1 means month 1 (January)
          # We need to aggregate monthly data to yearly totals per sector

          # Get unique sector IDs from layer names
          layer_names <- names(r_kg_m2_yr)
          sector_ids <- unique(stringr::str_match(layer_names, "sector=([0-9]+)_")[, 2])
          sector_ids <- sector_ids[!is.na(sector_ids)]

          # Process each sector
          sector_rasters <- list()
          for (sector_id in sector_ids) {
            # Find all layers for this sector (across all 12 months)
            sector_pattern <- paste0("sector=", sector_id, "_")
            sector_layers <- grep(sector_pattern, names(r_kg_m2_yr), value = TRUE)
            sector_name <- CEDS_MAP_SECTOR_MAPPING[[sector_id]]

            stopifnot(length(sector_layers)==12)

            if (length(sector_layers) > 0) {
              # Extract the layers for this sector
              sector_stack <- r_kg_m2_yr[[sector_layers]]

              # Average across all months to get yearly average flux
              sector_raster <- terra::app(sector_stack, mean, na.rm = TRUE)

              # Set the units attribute for yearly average flux
              terra::units(sector_raster) <- "kg m-2 yr-1"

              # Set layer name to sector ID
              names(sector_raster) <- sector_id

              sector_rasters[[sector_name]] <- sector_raster
            }
          }

          # Combine all sectors into a single raster stack
          if (length(sector_rasters) > 0) {
            # Combine rasters using terra::rast() with names
            processed_stack <- terra::rast(sector_rasters)

            # Save processed file
            filename <- basename(nc_file)
            dest_file <- file.path(self$data_dir, filename)

            terra::writeCDF(processed_stack, dest_file, overwrite = TRUE, split=TRUE, compress=9)
            processed_files[[length(processed_files) + 1]] <- dest_file

            message(glue::glue("Processed and saved: {filename}"))
          }
        }
      }

      return(invisible(processed_files))
    },

    #' @description Get sector ID from sector name or code
    #' @param sector Sector name or code
    #' @return Sector ID or NULL if not found
    get_sector_id = function(sector) {
      # If sector is already a sector ID
      if (sector %in% names(CEDS_MAP_SECTOR_MAPPING)) {
        return(sector)
      }

      # Try to find sector by name
      for (id in names(CEDS_MAP_SECTOR_MAPPING)) {
        if (CEDS_MAP_SECTOR_MAPPING[[id]] == sector) {
          return(id)
        }
      }

      # If sector is not found, try to use it directly as a sector ID
      return(sector)
    }
  )
)
