#' @title EDGARSourceProvincial
#' @description R6 class for EDGAR provincial emissions data
#'
#' @importFrom R6 R6Class
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year floor_date
#' @importFrom stringr str_extract str_replace
#' @export

# Sector mapping utilities are now in sector_mappings.R
EDGARSourceProvincial <- R6::R6Class(
  "EDGARSourceProvincial",
  inherit = EmissionsSourceProvincial,

  public = list(
    #' @description Create a new EDGARSourceProvincial object
    #' @param version Data version
    #' @param available_years Available years
    initialize = function(version = "v8.1",
                          available_years = 2000:2022) {
      super$initialize(
        source_name = "EDGAR",
        version = version,
        available_years = available_years
      )
    },

    #' @description Get EDGAR sectors mapping
    get_sectors = function() {
      sectors <- list(
        "ENE" = "Power industry",
        "REF_TRF" = "Oil refineries & Transformation industry",
        "IND" = "Combustion for manufacturing",
        "TNR_Aviation_CDS" = "Aviation climbing & descent",
        "TNR_Aviation_CRS" = "Aviation cruise",
        "TNR_Aviation_LTO" = "Aviation landing & take-off",
        "TNR_Aviation_SPS" = "Aviation supersonic",
        "TRO" = "Road transport",
        "TNR_Other" = "Railways, pipelines, off-road transport",
        "TNR_Ship" = "Shipping",
        "RCO" = "Energy for buildings",
        "PRO_FFF" = "Fuel exploitation",
        "NMM" = "Non-metallic minerals production",
        "CHE" = "Chemical processes",
        "IRO" = "Iron and steel production",
        "NFE" = "Non-ferrous metal production",
        "NEU" = "Non-energy use of fuels",
        "PRU_SOL" = "Solvents and products use",
        "FOO_PAP" = "Food and paper",
        "MNM" = "Manure management",
        "AWB" = "Agricultural waste burning",
        "AGS" = "Agricultural soils",
        "SWD_LDF" = "Solid waste landfills",
        "SWD_INC" = "Solid waste incineration",
        "WWT" = "Waste water handling",
        "TOTALS" = "Total Emissions"
      )
      return(sectors)
    },

    #' @description Download gridded data for provincial analysis
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param sectors Vector of sectors to process (defaults to all sectors if NULL)
    #' @return List with gridded directory path and filtered files
    download_gridded_data = function(years = NULL,
                                    pollutants = c("BC", "CO", "NH3", "NMVOC", "NOX", "OC", "PM10", "PM25", "SO2"),
                                    sectors = NULL) {
      if (is.null(years)) {
        years <- self$available_years
      }
      if (is.null(sectors)) {
        sectors <- names(self$get_sectors())
      }
      dir_netcdf <- file.path(self$cache_dir, "gridded")
      if (!dir.exists(dir_netcdf)) {
        dir.create(dir_netcdf, recursive = TRUE, showWarnings = FALSE)
      }
      nc_files <- list()

      # Download each pollutant/sector combination once
      for (poll in pollutants) {
        for (sector in sectors) {
          message(glue::glue("Downloading EDGAR gridded data for {poll} {sector}"))
          nc_files_sector <- self$download_nc(poll, sector, dir_netcdf)
          if (length(nc_files_sector) > 0) {
            nc_files <- c(nc_files, nc_files_sector)
          }
        }
      }

      # Filter files by requested years
      if (length(nc_files) > 0) {
        nc_files <- self$filter_files_by_year(nc_files, years)
      }

      return(list(
        dir_netcdf = dir_netcdf,
        nc_files = nc_files
      ))
    },

    #' @description Download a single netCDF file
    #' @param pollutant Pollutant to download
    #' @param sector Sector to download
    #' @param dir Directory to save the file
    #' @return Path to downloaded file or NULL if download failed
    download_nc = function(pollutant, sector, dir) {
      dir.create(dir, showWarnings = FALSE)
      edgar_poll <- switch(pollutant,
                          "NOx" = "NOX",
                          "BC" = "BC",
                          "CH4" = "CH4",
                          "CO" = "CO",
                          "CO2" = "CO2",
                          "N2O" = "N2O",
                          "NH3" = "NH3",
                          "NMVOC" = "NMVOC",
                          "OC" = "OC",
                          "SO2" = "SO2",
                          pollutant)

      url <- glue::glue("https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v81_FT2022_AP_new/{edgar_poll}/{sector}/{sector}_emi_nc.zip")
      dest_file_zip <- glue::glue("{dir}/{edgar_poll}_{sector}_v{self$version}.zip")

      # Check if we already have extracted .nc files for this pollutant/sector
      existing_files <- list.files(dir, pattern = glue::glue("{edgar_poll}.*{sector}.*\\.nc$"), full.names = TRUE)
      if (length(existing_files) > 0) {
        message(glue::glue("Files already exist for {pollutant} {sector}"))
        return(existing_files)
      }

      # Check if zip file exists and is valid
      if (file.exists(dest_file_zip) && file.info(dest_file_zip)$size > 1e6) {
        message(glue::glue("Zip file exists for {pollutant} {sector}, extracting..."))
        tryCatch({
          unzip(dest_file_zip, exdir = dir)
          nc_files <- list.files(dir, pattern = glue::glue("{edgar_poll}.*{sector}.*\\.nc$"), full.names = TRUE)
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
        unzip(dest_file_zip, exdir = dir)

        # Find all .nc files for this pollutant/sector
        nc_files <- list.files(dir, pattern = glue::glue("{edgar_poll}.*{sector}.*\\.nc$"), full.names = TRUE)

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

    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @return Data frame with provincial emissions
    extract_emissions_from_grid = function(vect, gridded_data, iso2, preserve_sector_codes = FALSE) {
      message("Creating terra stack from all netCDF files...")
      
      # 1- Create a single stack from all netCDF files
      stack_list <- list()
      file_metadata <- list()
      
      for (nc_file in gridded_data$nc_files) {
        # Extract pollutant and year from filename
        filename <- basename(nc_file)
        # Parse filename like: v8.1_FT2022_AP_NMVOC_2022_ENE_emi.nc
        parts <- strsplit(filename, "_")[[1]]
        pollutant <- parts[4]  # NMVOC
        year <- as.numeric(parts[5])  # 2022

        # Extract sector from filename
        base_name <- str_replace(filename, "_emi\\.nc$", "")
        parts <- strsplit(base_name, "_")[[1]]
        year_pos <- which(parts == as.character(year))
        if (length(year_pos) > 0 && year_pos < length(parts)) {
          sector_code <- paste(parts[(year_pos + 1):length(parts)], collapse = "_")
        } else {
          sector_code <- parts[6]
        }

        # Read netCDF file
        nc <- ncdf4::nc_open(nc_file)
        r <- terra::rast(nc_file)
        
        # Check units and apply conversion
        units <- terra::units(r)
        message(glue::glue("Processing {filename}: {units[1]}"))
        
        if (all(units == "kg m-2 s-1")) {
          # Convert from kg/m2/s to kg/s
          area_r_m2 <- terra::cellSize(r, unit="m")
          r_kg_s <- r * area_r_m2
          conversion_factor <- 365 * 24 * 3600 / 1e6
        } else if (all(units == "Tonnes")) {
          # EDGAR files are already in tonnes, just need to convert to kt/year
          r_kg_s <- r
          conversion_factor <- 1 / 1e3  # tonnes to kt
        } else {
          warning(glue::glue("Unexpected units for {filename}: {units[1]}"))
          r_kg_s <- r
          conversion_factor <- 1
        }
        
        # Set meaningful layer names for the stack
        layer_names <- paste0(pollutant, "_", sector_code, "_", year)
        names(r_kg_s) <- layer_names
        
        # Store metadata for each layer
        for (i in seq_along(layer_names)) {
          file_metadata[[layer_names[i]]] <- list(
            pollutant = pollutant,
            sector = sector_code,
            year = year,
            conversion_factor = conversion_factor,
            filename = filename
          )
        }
        
        stack_list[[length(stack_list) + 1]] <- r_kg_s
        ncdf4::nc_close(nc)
      }
      
      # Combine all rasters into a single stack
      if (length(stack_list) == 0) {
        stop("No valid netCDF files found")
      }
      
      message(glue::glue("Creating stack from {length(stack_list)} files..."))
      combined_stack <- terra::rast(stack_list)
      
      # 2- Extract data once from the combined stack
      message("Extracting zonal statistics from combined stack...")
      zonal_results <- terra::extract(combined_stack, terra::makeValid(vect), fun="sum", ID=TRUE, exact=TRUE)
      
      # 3- Process results
      message("Processing extraction results...")
      emissions <- as_tibble(zonal_results, .name_repair="minimal") %>%
        pivot_longer(cols = -c("ID"),
                     names_to = "layer_name",
                     values_to = "value") %>%
        mutate(
          # Extract metadata from layer names
          pollutant = sapply(layer_name, function(x) file_metadata[[x]]$pollutant),
          sector = sapply(layer_name, function(x) file_metadata[[x]]$sector),
          year = sapply(layer_name, function(x) file_metadata[[x]]$year),
          conversion_factor = sapply(layer_name, function(x) file_metadata[[x]]$conversion_factor)
        ) %>%
        # Apply conversion factors
        mutate(
          value = value * conversion_factor,
          unit = "kt/year"
        ) %>%
        dplyr::select(-layer_name, -conversion_factor)

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
          region_level = level,
          # Add translated sector name for display
          sector_name = sapply(sector, function(s) get_sector_name(s, "EDGAR", "provincial"))
        ) %>%
        dplyr::select(iso3, region_id, region_name, region_level, poll = pollutant, year, unit, value, sector, sector_name)

      message(glue::glue("Extraction complete. Processed {nrow(emissions)} records from {length(stack_list)} files."))
      return(emissions)
    },

    #' @description Get available maps for EDGAR provincial data
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

    #' @description Get available NetCDF data combinations for EDGAR
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
              
              # Map NetCDF layer names back to sector names for EDGAR
              sector_mapping <- c(
                "energy" = "Energy",
                "industry" = "Industry",
                "transport" = "Transport",
                "residential" = "Residential and other sectors",
                "agriculture" = "Agriculture",
                "waste" = "Waste",
                "shipping" = "International shipping",
                "aviation" = "International aviation"
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

    #' @description Generate a map raster from EDGAR NetCDF data
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
      # Map sector names to NetCDF layer names for EDGAR
      # This mapping depends on your NetCDF file structure
      sector_mapping <- c(
        "Energy" = "energy",
        "Industry" = "industry",
        "Transport" = "transport",
        "Residential and other sectors" = "residential",
        "Agriculture" = "agriculture",
        "Waste" = "waste",
        "International shipping" = "shipping",
        "International aviation" = "aviation"
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
