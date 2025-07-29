#' @title CEDSSourceProvincial
#' @description R6 class for CEDS provincial emissions data
#'
#' @importFrom R6 R6Class
#' @export
CEDSSourceProvincial <- R6::R6Class(
  "CEDSSourceProvincial",
  inherit = EmissionsSourceProvincial,

  public = list(
    #' @description Create a new CEDSSourceProvincial object
    #' @param version Data version
    #' @param available_years Available years
    initialize = function(version = "2024_11_25",
                          available_years = 2022:2022) {
      super$initialize(
        source_name = "CEDS",
        version = version,
        available_years = available_years
      )
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

      # Create directory for downloaded files
      dir_netcdf <- file.path(self$cache_dir, "gridded")
      if (!dir.exists(dir_netcdf)) {
        dir.create(dir_netcdf, recursive = TRUE, showWarnings = FALSE)
      }

      # Download each pollutant for each year
      nc_files <- list()

      for (poll in pollutants) {
        for (year in years) {
          message(glue::glue("Downloading CEDS gridded data for {poll} in {year}"))

          # Download the netCDF file
          nc_file <- self$download_nc(year, poll, dir_netcdf)

          if (!is.null(nc_file)) {
            nc_files[[length(nc_files) + 1]] <- nc_file
          }
        }
      }

      return(list(
        dir_netcdf = dir_netcdf,
        nc_files = nc_files
      ))
    },

    #' @description Download a single netCDF file
    #' @param year Year to download
    #' @param pollutant Pollutant to download
    #' @param dir Directory to save the file
    #' @return Path to downloaded file or NULL if download failed
    download_nc = function(year, pollutant, dir) {
      dir.create(dir, showWarnings = FALSE)

      # Construct URL based on pollutant and year
      # https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_11_25/gridded_emissions/bulk_emissions/fine_grids/SO2/SO2-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-11-25_gn_202201-202212.nc
      url <- glue("https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_{gsub('-','_',self$version)}/gridded_emissions/bulk_emissions/fine_grids/{pollutant}/{pollutant}-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-{gsub('_','-',self$version)}_gn_{year}01-{year}12.nc")

      dest_file <- glue::glue("{dir}/{pollutant}_{year}_v{self$version}.nc")

      # Skip if file already exists and is large enough
      if (file.exists(dest_file) && file.info(dest_file)$size > 1.6e8) {
        message(glue::glue("File already exists: {dest_file}"))
        return(dest_file)
      }

      # Download with timeout settings
      options(timeout = 300)
      options(download.file.method = "libcurl", url.method = "libcurl")
      download.file(url, dest_file)
      return(dest_file)
    },

    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @param level Administrative level
    #' @param res Resolution
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
    },

    #' @description Get available maps for CEDS provincial data
    #' @return Data frame with available map combinations
    available_maps = function() {
      # Scan existing map files (TIFF) in maps directory
      maps_dir <- file.path(self$data_dir, "maps")
      
      if (!dir.exists(maps_dir)) {
        return(data.frame(
          iso2 = character(), 
          pollutant = character(), 
          sector = character(), 
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }
      
      # Get all TIF files in maps directory
      tif_files <- list.files(maps_dir, pattern = "\\.tif$", full.names = FALSE)
      
      if (length(tif_files) == 0) {
        return(data.frame(
          iso2 = character(), 
          pollutant = character(), 
          sector = character(), 
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }
      
      # Parse filenames to extract metadata
      # Expected format: pollutant_sector_year_iso3.tif
      available_data <- list()
      
      for (filename in tif_files) {
        # Remove .tif extension
        name_without_ext <- gsub("\\.tif$", "", filename)
        
        # Split by underscore
        parts <- strsplit(name_without_ext, "_")[[1]]
        
        if (length(parts) >= 4) {
          pollutant <- parts[1]
          sector <- parts[2]
          year <- as.integer(parts[3])
          iso3 <- parts[4]
          iso2 <- countrycode::countrycode(iso3, "iso3c", "iso2c")
          
          if (!is.na(iso2)) {
            available_data[[length(available_data) + 1]] <- data.frame(
              iso2 = iso2,
              pollutant = pollutant,
              sector = sector,
              year = year,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      
      if (length(available_data) == 0) {
        return(data.frame(
          iso2 = character(), 
          pollutant = character(), 
          sector = character(), 
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }
      
      result <- do.call(rbind, available_data)
      return(result)
    },

    #' @description Get a map raster for specific parameters
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso2 ISO2 country code
    #' @param save Whether to save the map file
    #' @return Terra raster object or file path if saved
    get_map = function(pollutant,
                      sector,
                      year,
                      iso2,
                      save = FALSE) {
      iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
      
      # Check if map file already exists
      maps_dir <- file.path(self$data_dir, "maps")
      filename <- paste0(tolower(pollutant), "_", tolower(sector), "_", year, "_", tolower(iso3), ".tif")
      filepath <- file.path(maps_dir, filename)
      
      if (file.exists(filepath)) {
        # Load existing map file
        if (save) {
          return(filepath)
        } else {
          return(terra::rast(filepath))
        }
      } else {
        # No map file exists, return NULL
        return(NULL)
      }
    },

    #' @description Clear all map files for this source
    #' @return Invisibly returns the number of files removed
    clear_maps = function() {
      maps_dir <- file.path(self$data_dir, "maps")
      if (!dir.exists(maps_dir)) {
        message("No maps directory found")
        return(invisible(0))
      }
      
      # Get all TIF files in maps directory
      tif_files <- list.files(maps_dir, pattern = "\\.tif$", full.names = TRUE)
      
      if (length(tif_files) == 0) {
        message("No map files found to clear")
        return(invisible(0))
      }
      
      # Remove all TIF files
      removed_count <- 0
      for (file in tif_files) {
        if (file.remove(file)) {
          removed_count <- removed_count + 1
        }
      }
      
      message(glue::glue("Cleared {removed_count} map files"))
      return(invisible(removed_count))
    },

    #' @description Get available NetCDF data combinations for CEDS
    #' @return Data frame with available NetCDF data combinations
    get_available_provincial_data = function() {
      # Scan existing NetCDF files
      available_data <- list()
      
      # Get all NC files in gridded data directory
      nc_dir <- file.path(self$data_dir, "gridded")
      if (!dir.exists(nc_dir)) {
        return(data.frame(
          iso2 = character(), 
          pollutant = character(), 
          sector = character(), 
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }
      
      nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)
      
      for (file in nc_files) {
        # Extract pollutant and year from filename
        filename <- basename(file)
        
        # Parse filename to extract pollutant and year
        # Assuming filename format: pollutant_year.nc or similar
        parts <- strsplit(filename, "_|\\.")[[1]]
        
        if (length(parts) >= 2) {
          pollutant <- parts[1]
          year <- as.integer(parts[2])
          
          if (!is.na(year)) {
            # Get available sectors from the NetCDF file
            tryCatch({
              nc_stack <- terra::rast(file)
              sector_names <- names(nc_stack)
              
              # Map NetCDF layer names back to sector names
              sector_mapping <- c(
                "agriculture" = "Agriculture",
                "energy" = "Energy Sector",
                "industrial" = "Industrial Sector",
                "transport" = "Transportation Sector",
                "residential" = "Residential, Commercial, Other",
                "solvents" = "Solvents production and application",
                "waste" = "Waste",
                "shipping" = "International Shipping"
              )
              
              for (layer_name in sector_names) {
                sector_name <- sector_mapping[layer_name]
                if (is.na(sector_name)) {
                  sector_name <- layer_name  # Use layer name as fallback
                }
                
                # Add all supported countries plus global
                supported_countries <- c("CN", "ID", "IN", "TH", "VN", "ZA", "wld")
                
                for (iso2 in supported_countries) {
                  available_data[[length(available_data) + 1]] <- data.frame(
                    iso2 = iso2,
                    pollutant = pollutant,
                    sector = sector_name,
                    year = year,
                    stringsAsFactors = FALSE
                  )
                }
              }
            }, error = function(e) {
              # Skip files that can't be read
              warning(glue::glue("Could not read NetCDF file {file}: {e$message}"))
            })
          }
        }
      }
      
      if (length(available_data) == 0) {
        return(data.frame(
          iso2 = character(), 
          pollutant = character(), 
          sector = character(), 
          year = integer(),
          stringsAsFactors = FALSE
        ))
      }
      
      result <- do.call(rbind, available_data)
      return(result)
    },

    #' @description Generate a map raster from CEDS NetCDF data
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso2 ISO2 country code (use "wld" for global)
    #' @param save Whether to save the map file
    #' @return Terra raster object or file path if saved
    generate_map = function(pollutant,
                           sector,
                           year,
                           iso2,
                           save = FALSE) {
      
      # Handle global version
      if (iso2 == "wld") {
        iso3 <- "wld"
      } else {
        # Convert ISO2 to ISO3
        iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
      }
      
      # Get the NetCDF file for this pollutant and year
      nc_file <- self$get_nc_file(pollutant, year)
      if (is.null(nc_file) || !file.exists(nc_file)) {
        warning(glue::glue("No NetCDF file found for {pollutant} {year}"))
        return(NULL)
      }
      
      # Load the NetCDF file
      nc_stack <- terra::rast(nc_file)
      
      # Get the layer for the specific sector
      sector_layer <- self$get_sector_layer_name(sector)
      if (!sector_layer %in% names(nc_stack)) {
        warning(glue::glue("Sector layer '{sector_layer}' not found in NetCDF file"))
        return(NULL)
      }
      
      # Extract the sector layer
      sector_raster <- nc_stack[[sector_layer]]
      
      # Convert units from kg/s to kg/m²/s (assuming the data is in kg/s)
      # This depends on the actual units in your NetCDF files
      # You may need to adjust this conversion based on your data
      sector_raster <- sector_raster / terra::cellSize(sector_raster, unit = "m")
      
      if (iso2 == "wld") {
        # Global version - no cropping
        masked_raster <- sector_raster
      } else {
        # Get country boundaries for cropping
        country_boundaries <- self$get_country_boundaries(iso2)
        if (is.null(country_boundaries)) {
          warning(glue::glue("No country boundaries found for {iso2}"))
          return(NULL)
        }
        
        # Crop to country boundaries
        cropped_raster <- terra::crop(sector_raster, country_boundaries)
        masked_raster <- terra::mask(cropped_raster, country_boundaries)
      }
      
      # Set layer name
      names(masked_raster) <- paste(pollutant, sector, year, iso2, sep = "_")
      
      if (save) {
        # Create maps directory
        maps_dir <- file.path(self$data_dir, "maps")
        if (!dir.exists(maps_dir)) {
          dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
        }
        
        # Save as GeoTIFF
        filename <- paste0(tolower(pollutant), "_", tolower(sector), "_", year, "_", tolower(iso3), ".tif")
        filepath <- file.path(maps_dir, filename)
        
        terra::writeRaster(masked_raster, filepath, overwrite = TRUE)
        return(filepath)
      }
      
      return(masked_raster)
    },

    #' @description Get NetCDF file path for a pollutant and year
    #' @param pollutant Pollutant code
    #' @param year Year
    #' @return Path to NetCDF file or NULL if not found
    get_nc_file = function(pollutant, year) {
      # Look for NetCDF files in the gridded data directory
      nc_dir <- file.path(self$data_dir, "gridded")
      if (!dir.exists(nc_dir)) {
        return(NULL)
      }
      
      # Search for files matching the pattern
      pattern <- paste0(tolower(pollutant), ".*", year, ".*\\.nc$")
      nc_files <- list.files(nc_dir, pattern = pattern, full.names = TRUE)
      
      if (length(nc_files) == 0) {
        return(NULL)
      }
      
      return(nc_files[1])  # Return the first match
    },

    #' @description Get sector layer name for NetCDF files
    #' @param sector Sector name
    #' @return Layer name in NetCDF file
    get_sector_layer_name = function(sector) {
      # Map sector names to NetCDF layer names
      # This mapping depends on your NetCDF file structure
      sector_mapping <- c(
        "Agriculture" = "agriculture",
        "Energy Sector" = "energy",
        "Industrial Sector" = "industrial",
        "Transportation Sector" = "transport",
        "Residential, Commercial, Other" = "residential",
        "Solvents production and application" = "solvents",
        "Waste" = "waste",
        "International Shipping" = "shipping"
      )
      
      if (sector %in% names(sector_mapping)) {
        return(sector_mapping[sector])
      }
      
      # If not found, try lowercase version
      return(tolower(gsub(" ", "_", sector)))
    },

    #' @description Get country boundaries for cropping
    #' @param iso2 ISO2 country code
    #' @return Terra vector object or NULL if not found
    get_country_boundaries = function(iso2) {
      tryCatch({
        # Use creahelpers to get country boundaries
        boundaries <- terra::vect(creahelpers::get_adm(level = 0, res = "low", iso2s = iso2))
        return(boundaries)
      }, error = function(e) {
        warning(glue::glue("Could not get boundaries for {iso2}: {e$message}"))
        return(NULL)
      })
    }
  )
)
