#' @title CEDSProvincial
#' @description R6 class for CEDS provincial emissions data
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr distinct rename filter
#' @importFrom magrittr %>%
#' @export
CEDSProvincial <- R6::R6Class(
  "CEDSProvincial",
  inherit = SourceProvincial,

  public = list(
    #' @field version Data version
    version = NULL,

    #' @field available_years Available years
    available_years = NULL,

    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @description Create a new CEDSProvincial object
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Data directory path
    initialize = function(version = "2024_11_25",
                          available_years = 2022:2022,
                          data_dir = NULL) {
      # Use path resolution if data_dir is not provided
      if (is.null(data_dir)) {
        data_dir <- get_data_path(c("ceds", "provincial"))
      }

      # Create map source
      map_source <- CEDSMap$new(
        version = version,
        available_years = available_years
      )

      super$initialize(data_dir = data_dir, map_source = map_source)
      self$version <- version
      self$available_years <- available_years
      self$cache_dir <- file.path(get_project_root(), "cache", "ceds")

      # Create directories if they don't exist
      for (dir in c(self$data_dir, self$cache_dir)) {
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
    },

    #' @description Build provincial emissions data
    #' @param iso2s ISO2 country codes to process
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param level Administrative level
    #' @param res Resolution
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Invisibly returns paths to saved files
    build = function(iso2s,
                    years = NULL,
                    pollutants = CEDS_POLLUTANTS,
                    level = 1,
                    res = "low",
                    buffer_into_sea_km = 20) {
      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Extract provincial data
      emissions <- self$extract_provincial_data(
        iso2s = iso2s,
        years = years,
        pollutants = pollutants,
        level = level,
        res = res,
        buffer_into_sea_km = buffer_into_sea_km
      )

      # Save provincial data
      results <- self$save_provincial_data(emissions)

      message("CEDS provincial data build complete!")
      return(invisible(results))
    },

    #' @description List available data combinations
    #' @param year Optional year filter
    #' @param sector Optional sector filter
    #' @param pollutant Optional pollutant filter
    #' @return Data frame with available pollutant/sector/year/iso3 combinations
    list_available_data = function(year = NULL, sector = NULL, pollutant = NULL) {
      if (!dir.exists(self$data_dir)) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Get all RDS files in provincial data directory and subdirectories
      rds_files <- list.files(self$data_dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)

      if (length(rds_files) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Read all files and combine data
      available_data <- list()

      for (file in rds_files) {
        tryCatch({
          data <- readRDS(file)
          if (nrow(data) > 0) {
            # Extract unique combinations
            combinations <- data %>%
              dplyr::distinct(poll, sector, year, iso3) %>%
              dplyr::rename(pollutant = poll)

            available_data[[length(available_data) + 1]] <- combinations
          }
        }, error = function(e) {
          # Skip files that can't be read
          warning(glue::glue("Could not read RDS file {file}: {e$message}"))
        })
      }

      if (length(available_data) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
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

    #' @description Get emissions data
    #' @param pollutant Pollutant code (can be NULL to get all pollutants)
    #' @param sector Sector code (can be NULL to get all sectors)
    #' @param year Year (can be NULL to get all years)
    #' @param iso3 ISO3 country code (can be NULL to get all countries)
    #' @return Data frame with emissions data or NULL if not available
    get = function(pollutant = NULL, sector = NULL, year = NULL, iso3 = NULL) {
      # Try by_year files first
      if (!is.null(year)) {
        by_year_file <- file.path(self$data_dir, "by_year", paste0("_provincial_", year, ".rds"))

        if (file.exists(by_year_file)) {
          data <- readRDS(by_year_file)

          # Apply filters only if parameters are not NULL
          filtered_data <- data
          if (!is.null(pollutant)) {
            filtered_data <- filtered_data %>% dplyr::filter(poll %in% !!pollutant)
          }
          if (!is.null(sector)) {
            filtered_data <- filtered_data %>% dplyr::filter(sector %in% !!sector)
          }
          if (!is.null(iso3)) {
            filtered_data <- filtered_data %>% dplyr::filter(tolower(iso3) %in% tolower(!!iso3))
          }

          if (nrow(filtered_data) > 0) {
            return(self$format_results(filtered_data))
          }
        }
      }

      # Try direct country files
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
        rds_files <- list.files(self$data_dir, pattern = "\\.rds$", full.names = TRUE)
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

      message(glue::glue("Cleared {removed_count} CEDS provincial data files"))
      return(invisible(removed_count))
    },

    #' @description Download gridded data for provincial analysis
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @return List with gridded directory path and filtered files
    download_gridded_data = function(years = NULL,
                                    pollutants = c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2")) {
      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Use the gridded directory for processed files
      dir_netcdf <- file.path(self$cache_dir, "gridded")
      if (!dir.exists(dir_netcdf)) {
        dir.create(dir_netcdf, recursive = TRUE, showWarnings = FALSE)
      }

      # Get cached files for each pollutant and year
      nc_files <- list()

      for (poll in pollutants) {
        for (year in years) {
          message(glue::glue("Getting CEDS gridded data for {poll} in {year}"))

          # Get the netCDF file from cache (download if not available)
          nc_file <- self$map_source$download_nc(poll, year)

          if (!is.null(nc_file) && file.exists(nc_file)) {
            nc_files[[length(nc_files) + 1]] <- nc_file
            message(glue::glue("Using cached file: {basename(nc_file)}"))
          } else {
            message(glue::glue("Failed to get file for {poll} {year}"))
          }
        }
      }

      return(list(
        dir_netcdf = dir_netcdf,
        nc_files = nc_files
      ))
    },

    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @param preserve_sector_codes Whether to preserve original sector codes (default: FALSE)
    #' @return Data frame with provincial emissions
    extract_emissions_from_grid = function(vect, gridded_data, iso2, preserve_sector_codes = FALSE) {
      message("Creating terra stack from all CEDS netCDF files...")

      # 1- Create a single stack from all netCDF files
      stack_list <- list()
      file_metadata <- list()

      for (nc_file in gridded_data$nc_files) {
        # Extract pollutant and year from filename
        filename <- basename(nc_file)
        parts <- strsplit(filename, "_")[[1]]
        pollutant <- parts[1]
        year <- as.numeric(gsub("\\.nc$", "", parts[2]))

        # Read netCDF file
        nc <- ncdf4::nc_open(nc_file)
        r <- terra::rast(nc_file)

        # Get sector labels from netCDF attributes
        # "0: Agriculture; 1: Energy; 2: Industrial; 3: Transportation ...
        sector_legend <- ncdf4::ncatt_get(nc, "sector", "ids")$value
        sector_labels <- strsplit(sector_legend, ";")[[1]]
        sector_ids <- trimws(str_extract(sector_labels, "^[ |0-9]+"))
        sector_names <- trimws(sub("^[ |0-9]+:\\s*", "", sector_labels))

        # Make sure this matches our mapping CEDS_PROVINCIAL_SECTORS
        # These should match CEDS_PROVINCIAL_SECTORS in 01_sector_mappings.R
        stopifnot(
          all(sector_ids %in% names(CEDS_PROVINCIAL_SECTORS)),
          all(sector_names == unname(CEDS_PROVINCIAL_SECTORS[sector_ids]))
        )

        # Use CEDS provincial sector mapping for consistent naming
        sector_names <- sapply(sector_ids, function(id) {
          if (preserve_sector_codes) {
            # For validation purposes, preserve original codes
            return(id)
          } else {
            # For display purposes, use mapped names
            return(get_sector_name(id, "CEDS", "provincial"))
          }
        })

        # Convert to kg/s
        area_r_m2 <- terra::cellSize(r, unit="m")
        message(glue::glue("Processing {filename}: converting from kg/m2/s to kg/s"))
        r_kg_s <- r * area_r_m2

        # CEDS files contain monthly data (12 months) for each sector
        # Layer names are like "NMVOC_em_anthro_sector=0_1" where:
        # - sector=0 means sector 0
        # - _1 means month 1 (January)
        # We need to aggregate monthly data to yearly totals

        # Get layer names to understand the structure
        layer_names <- names(r_kg_s)

        # Group layers by sector and aggregate monthly data to yearly
        sector_yearly_data <- list()

        for (sector_id in sector_ids) {
          # Find all layers for this sector (across all 12 months)
          sector_pattern <- paste0("sector=", sector_id, "_")
          sector_layers <- grep(sector_pattern, layer_names, value = TRUE)

          if (length(sector_layers) > 0) {
            # Extract the layers for this sector
            sector_stack <- r_kg_s[[sector_layers]]

            # Average across all months to get yearly average flux
            yearly_average <- terra::app(sector_stack, mean, na.rm = TRUE)

            # Create meaningful layer name
            layer_name <- paste0(pollutant, "_", sector_id, "_", year)

            # Store metadata
            file_metadata[[layer_name]] <- list(
              pollutant = pollutant,
              sector = sector_names[which(sector_ids == sector_id)],
              sector_id = sector_id,
              year = year,
              filename = filename
            )

            # Set layer name
            names(yearly_average) <- layer_name
            sector_yearly_data[[length(sector_yearly_data) + 1]] <- yearly_average
          }
        }

        stack_list[[length(stack_list) + 1]] <- terra::rast(sector_yearly_data)
        ncdf4::nc_close(nc)
      }

      # Combine all rasters into a single stack
      if (length(stack_list) == 0) {
        stop("No valid CEDS netCDF files found")
      }

      message(glue::glue("Creating stack from {length(stack_list)} files..."))
      combined_stack <- terra::rast(stack_list)

      # 2- Extract data once from the combined stack
      message("Extracting zonal statistics from combined CEDS stack...")
      zonal_results <- terra::extract(combined_stack, terra::makeValid(vect), fun="sum", ID=TRUE, exact=TRUE)

      # 3- Process results
      message("Processing CEDS extraction results...")
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
          value = value * 24 * 3600 * 365 / 1e6,  # Convert from kg/s to kt/year
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

      message(glue::glue("CEDS extraction complete. Processed {nrow(emissions)} records from {length(stack_list)} files."))
      return(emissions)
    }
  )
)
