#' @title EDGARSourceProvincial
#' @description R6 class for EDGAR provincial emissions data
#'
#' @importFrom R6 R6Class
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year floor_date
#' @importFrom stringr str_extract str_replace
#' @export

# Source sector mapping utilities
if (!exists("get_edgar_gridded_sector_name")) {
  source("edgar_sectors.R")
}
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
          sector_name = sapply(sector, get_edgar_gridded_sector_name)
        ) %>%
        dplyr::select(iso3, region_id, region_name, region_level, poll = pollutant, year, unit, value, sector, sector_name)

      message(glue::glue("Extraction complete. Processed {nrow(emissions)} records from {length(stack_list)} files."))
      return(emissions)
    }
  )
)
