#' @title EDGARProvincial
#' @description R6 class for EDGAR provincial emissions data
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr distinct rename filter %>%
#' @importFrom tidyr crossing pivot_longer
#' @importFrom lubridate year floor_date
#' @importFrom stringr str_extract str_replace
#' @export

# Sector mapping utilities are now in sector_mappings.R
EDGARProvincial <- R6::R6Class(
  "EDGARProvincial",
  inherit = SourceProvincial,

  public = list(
    #' @field version Data version
    version = NULL,

    #' @field available_years Available years
    available_years = NULL,

    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @field available_data_cache Cached available data combinations
    available_data_cache = NULL,

    #' @description Create a new EDGARProvincial object
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Data directory path
    initialize = function(version = "v8.1",
                          available_years = 2000:2022,
                          data_dir = NULL) {
      # Use path resolution if data_dir is not provided
      if (is.null(data_dir)) {
        data_dir <- get_data_path(c("edgar", "provincial"))
      }

      # Create map source
      map_source <- EDGARMap$new(
        version = version,
        available_years = available_years
      )

      super$initialize(data_dir = data_dir, map_source = map_source)
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

    #' @description Format results to standard format with sector and sector_group columns
    #' @param data Data frame to format
    #' @return Formatted data frame with sector and sector_group columns added
    format_results = function(data) {
      # Call parent format_results first
      data <- super$format_results(data)

      # Step 1: Apply SECTOR_MAPPING to create readable sector names
      data$sector <- map_values(data$sector, EDGAR_PROVINCIAL_SECTOR_MAPPING)

      # Step 2: Apply SECTOR_GROUP_MAPPING to create sector groups
      data$sector_group <- map_values(data$sector, EDGAR_PROVINCIAL_SECTOR_GROUP_MAPPING)

      return(data)
    },

    #' @description Build provincial emissions data
    #' @param iso2s ISO2 country codes to process
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param level Administrative level
    #' @param res Resolution
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Invisibly returns paths to saved files
    build = function(
      iso2s,
      years = NULL,
      pollutants = names(EDGAR_POLLUTANTS),
      sectors = names(EDGAR_PROVINCIAL_SECTOR_MAPPING),
      level = 1,
      res = "low",
      buffer_into_sea_km = 20) {

      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Build map data first
      # self$map_source$build(pollutants = pollutants, years = years)

      # Extract provincial data
      emissions <- self$extract_provincial_data(
        iso2s = iso2s,
        years = years,
        pollutants = pollutants,
        level = level,
        res = res,
        buffer_into_sea_km = buffer_into_sea_km,
        sectors = sectors
      )

      # Save provincial data
      results <- self$save_provincial_data(emissions)

      message("EDGAR provincial data build complete!")
      return(invisible(results))
    },

    #' @description List available data combinations
    #' @param year Optional year filter
    #' @param sector Optional sector filter
    #' @param pollutant Optional pollutant filter
    #' @return Data frame with available pollutant/sector/year/iso3 combinations
    list_available_data = function(year = NULL, sector = NULL, pollutant = NULL) {
      # Check cache first - if we have cached data and no filters, return it immediately
      if (!is.null(self$available_data_cache) && is.null(pollutant) && is.null(year) && is.null(sector)) {
        return(self$available_data_cache)
      }

      if (!dir.exists(self$data_dir)) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Get all RDS files in provincial data directory
      rds_files <- list.files(self$data_dir, pattern = "\\.rds$", full.names = TRUE)

      if (length(rds_files) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Extract available countries from filenames (much faster than reading all files)
      # Provincial files are typically named like: edgar_emissions_id.rds, edgar_emissions_cn.rds, etc.
      # Handle various naming patterns and ensure clean ISO3 codes
      available_countries <- unique(gsub(".*_emissions_([a-z]{2,3})\\.rds", "\\1", basename(rds_files)))
      available_countries <- available_countries[available_countries != ""] # Remove any empty matches

            # Additional cleanup: ensure we have valid ISO3 codes and remove any remaining file extensions
      available_countries <- gsub("\\.rds$", "", available_countries, ignore.case = TRUE)
      available_countries <- tolower(available_countries) # Ensure lowercase

      # Validate that we have clean ISO3 codes (should be 2-3 lowercase letters)
      available_countries <- available_countries[grepl("^[a-z]{2,3}$", available_countries)]

      # Debug: show what we extracted
      if (length(available_countries) > 0) {
        message("EDGAR provincial: Extracted countries: ", paste(available_countries, collapse = ", "))
      }

      # Read only ONE file to get the structure (sectors, pollutants, years are the same across countries)
      sample_file <- rds_files[1]

      tryCatch({
        sample_data <- readRDS(sample_file)

        if (nrow(sample_data) > 0) {
          # Extract unique combinations from the sample file
          base_combinations <- sample_data %>%
            dplyr::distinct(poll, sector, year) %>%
            dplyr::rename(pollutant = poll)

          # Create all combinations by crossing with available countries
          result <- base_combinations %>%
            tidyr::crossing(iso3 = available_countries)

        } else {
          # Fallback to empty data frame
          result <- data.frame(
            pollutant = character(),
            sector = character(),
            year = integer(),
            iso3 = character(),
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        # Fallback to empty data frame if file can't be read
        warning(glue::glue("Could not read sample RDS file {sample_file}: {e$message}"))
        result <- data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        )
      })

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

      # Cache the result for future calls (only if no filters applied)
      if (is.null(pollutant) && is.null(year) && is.null(sector)) {
        self$available_data_cache <- result
      }

      return(result)
    },

    #' @description Clear the available data cache
    clear_cache = function() {
      self$available_data_cache <- NULL
      message("EDGAR provincial: Cache cleared")
    },

    #' @description Get emissions data
    #' @param pollutant Pollutant code (can be NULL to get all pollutants)
    #' @param sector Sector code (can be NULL to get all sectors)
    #' @param year Year (can be NULL to get all years)
    #' @param iso3 ISO3 country code (can be NULL to get all countries)
    #' @return Data frame with emissions data or NULL if not available
    get = function(pollutant = NULL, sector = NULL, year = NULL, iso3 = NULL) {
      if (!is.null(iso3)) {
        target_iso3 <- tolower(iso3)
        filepath <- file.path(self$data_dir, paste0(target_iso3, ".rds"))

        if (!file.exists(filepath)) {
          return(NULL)
        }

        data <- readRDS(filepath)

        # Apply filters only if parameters are not NULL
        filtered_data <- data
        if (!is.null(pollutant)) {
          filtered_data <- filtered_data %>% dplyr::filter(poll == !!pollutant)
        }
        if (!is.null(sector)) {
          filtered_data <- filtered_data %>% dplyr::filter(sector == !!sector)
        }
        if (!is.null(year)) {
          filtered_data <- filtered_data %>% dplyr::filter(year == !!year)
        }

        if (nrow(filtered_data) > 0) {
          return(self$format_results(filtered_data))
        }
      } else {
        # Get all country files if no country specified
        rds_files <- list.files(self$data_dir, pattern = "\\.rds$", full.names = TRUE)

        all_data <- list()
        for (file in rds_files) {
          data <- readRDS(file)

          # Apply filters only if parameters are not NULL
          filtered_data <- data
          if (!is.null(pollutant)) {
            filtered_data <- filtered_data %>% dplyr::filter(poll == !!pollutant)
          }
          if (!is.null(sector)) {
            filtered_data <- filtered_data %>% dplyr::filter(sector == !!sector)
          }
          if (!is.null(year)) {
            filtered_data <- filtered_data %>% dplyr::filter(year == !!year)
          }

          if (nrow(filtered_data) > 0) {
            all_data[[length(all_data) + 1]] <- filtered_data
          }
        }

        if (length(all_data) > 0) {
          return(self$format_results(dplyr::bind_rows(all_data)))
        }
      }

      return(NULL)
    },

    #' @description Clear all built data
    #' @return Invisibly returns the number of files removed
    clear = function() {
      removed_count <- 0

      # Clear provincial data files
      if (dir.exists(self$data_dir)) {
        rds_files <- list.files(self$data_dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
        for (file in rds_files) {
          if (file.remove(file)) {
            removed_count <- removed_count + 1
          }
        }
      }

      # Clear map data
      if (!is.null(self$map_source)) {
        self$map_source$clear()
      }

      # Clear cache
      self$clear_cache()

      message(glue::glue("Cleared {removed_count} EDGAR provincial data files"))
      return(invisible(removed_count))
    },


    #' @description Extract provincial data from gridded data
    #' @param iso2s ISO2 country codes
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param level Administrative level
    #' @param res Resolution ("low" or "high")
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Data frame with provincial emissions
    extract_provincial_data = function(iso2s,
                                      years = NULL,
                                      pollutants = names(EDGAR_POLLUTANTS),
                                      sectors = names(EDGAR_PROVINCIAL_SECTOR_MAPPING),
                                      level = 1,
                                      res = "low",
                                      buffer_into_sea_km = 20) {

      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Process each country separately to avoid memory issues
      emissions <- lapply(iso2s, function(iso2) {
        iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
        filepath <- file.path(self$data_dir, paste0(tolower(iso3), ".rds"))

        # Return existing data if available
        if (file.exists(filepath)) {
          message(glue::glue("Loading existing provincial data for {iso2}"))
          return(readRDS(filepath))
        }

        message(glue::glue("Extracting emissions for {iso2}"))

        # Get province boundaries
        vect <- self$get_province_boundaries(iso2, level, res, buffer_into_sea_km)

        # Download and prepare gridded data using map source
        gridded_data <- self$download_gridded_data(years, pollutants, sectors)

        # Extract emissions for each province
        result <- self$extract_emissions_from_grid(vect, gridded_data, iso2)

        # Save result
        saveRDS(result, filepath)
        return(result)
      }) %>%
        dplyr::bind_rows()

      return(emissions)
    },

    #' @description Download gridded data for provincial analysis
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param sectors Vector of sectors to process (defaults to all sectors if NULL)
    #' @return List with gridded directory path and filtered files
    download_gridded_data = function(years = NULL,
                                    pollutants = names(EDGAR_POLLUTANTS),
                                    sectors = NULL) {
      if (is.null(years)) {
        years <- self$available_years
      }

      if (is.null(sectors)) {
        sectors <- names(EDGAR_PROVINCIAL_SECTOR_MAPPING)
      }

      dir_netcdf <- file.path(self$cache_dir, "gridded")
      if (!dir.exists(dir_netcdf)) {
        dir.create(dir_netcdf, recursive = TRUE, showWarnings = FALSE)
      }
      nc_files <- list()

      # Download each pollutant/sector/year combination
      for (poll in pollutants) {
        for (sector in sectors) {
            message(glue::glue("Downloading EDGAR gridded data for {poll} {sector}"))
            nc_files_poll_sector <- self$map_source$download_nc(poll, sector)
            nc_files_poll_sector_year <- self$map_source$filter_files_by_year(nc_files_poll_sector, years)
            if (length(nc_files_poll_sector_year) > 0) {
              nc_files <- c(nc_files, nc_files_poll_sector_year)
            }
          }
      }

      # Files are already filtered by year since we download specific years
      # No need to filter again

      return(list(
        dir_netcdf = dir_netcdf,
        nc_files = nc_files
      ))
    },

    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @return Data frame with provincial emissions
    extract_emissions_from_grid = function(vect, gridded_data, iso2) {
      message("Creating terra stack from all EDGAR netCDF files...")

      # 1- Create a single stack from all netCDF files
      stack_list <- list()
      file_metadata <- list()

      for (nc_file in gridded_data$nc_files) {
        # Extract pollutant, year, and sector from filename
        filename <- basename(nc_file)
        # Parse EDGAR filename pattern: v8.1_FT2022_AP_NMVOC_1970_RCO_emi.nc
        parts <- strsplit(filename, "_")[[1]]

        if (length(parts) >= 6) {
          pollutant <- parts[4]  # NMVOC
          pollutant <- map_values(pollutant, EDGAR_POLLUTANTS)
          year <- as.numeric(parts[5])  # 1970
          sector <- parts[6]  # RCO

          # Read netCDF file
          r_tonnes <- terra::rast(nc_file) %>%
            terra::crop(vect)

          # Create meaningful layer name
          layer_name <- paste0(pollutant, "_", sector, "_", year)

          # Store metadata
          file_metadata[[layer_name]] <- list(
            pollutant = pollutant,
            sector = sector,
            year = year,
            filename = filename
          )

          # Set layer name
          names(r_tonnes) <- layer_name
          stack_list[[length(stack_list) + 1]] <- r_tonnes
        } else {
          message(glue::glue("Skipping {filename}: unexpected filename format"))
        }
      }

      # Combine all rasters into a single stack
      if (length(stack_list) == 0) {
        stop("No valid EDGAR netCDF files found")
      }

      message(glue::glue("Creating stack from {length(stack_list)} files..."))
      combined_stack <- terra::rast(stack_list)

      # 2- Extract data once from the combined stack
      message("Extracting zonal statistics from combined EDGAR stack...")
      zonal_results <- terra::extract(combined_stack, terra::makeValid(vect), fun=sum, ID=TRUE, exact=TRUE, weights=TRUE)

      # 3- Process results
      message("Processing EDGAR extraction results...")
      emissions <- as_tibble(zonal_results, .name_repair="minimal") %>%
        pivot_longer(cols = -c("ID"),
                     names_to = "layer_name",
                     values_to = "value") %>%
        mutate(
          # Extract metadata from layer names
          pollutant = sapply(layer_name, function(x) file_metadata[[x]]$pollutant),
          sector = sapply(layer_name, function(x) file_metadata[[x]]$sector),
          year = sapply(layer_name, function(x) file_metadata[[x]]$year)
        ) %>%
        mutate(
          value = value / 1e3,  # Convert t/year to kt/year
          unit = "kt/year"
        ) %>%
        dplyr::select(-layer_name)

      # 4- Add region information
      level <- 1
      colnames <- setdiff(c(paste0("GID_", 0:level), paste0("NAME_", 0:level)), "NAME_0")

      emissions <- vect %>%
        as.data.frame() %>%
        mutate(ID = row_number()) %>%
        distinct_at(c("ID", colnames)) %>%
        left_join(emissions, by = "ID",
                  relationship = "many-to-many") %>%
        dplyr::select(-ID) %>%
        mutate(
          iso3 = tolower(GID_0),
          region_id = GID_1,
          region_name = NAME_1,
          region_level = level
        ) %>%
        dplyr::select(iso3, region_id, region_name, region_level, poll = pollutant, sector, year, unit, value)

      message(glue::glue("EDGAR extraction complete. Processed {nrow(emissions)} records from {length(stack_list)} files."))
      return(emissions)
    }
  )
)
