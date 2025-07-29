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

        # Use CEDS sector mapping for consistent naming
        sector_names <- sapply(sector_ids, function(id) {
          if (preserve_sector_codes) {
            # For validation purposes, preserve original codes
            return(id)
          } else if (id %in% names(CEDS_GRIDDED_SECTORS)) {
            # For display purposes, use mapped names
            return(CEDS_GRIDDED_SECTORS[id])
          }
          return(id)
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
