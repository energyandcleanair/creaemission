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
    initialize = function(version = "v_2024_04_01",
                          available_years = 2000:2022) {
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
      # https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/BC/BC-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-07-08_gr_198001-198012.nc

      url <- glue("https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/{pollutant}/{pollutant}-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-07-08_gr_{year}01-{year}12.nc")

      dest_file <- glue::glue("{dir}/{pollutant}_{year}.nc")

      # Skip if file already exists and is large enough
      if (file.exists(dest_file) && file.info(dest_file)$size > 1.6e8) {
        message(glue::glue("File already exists: {dest_file}"))
        return(dest_file)
      }

      # Download with timeout settings
      options(timeout = 300)
      options(download.file.method = "libcurl", url.method = "libcurl")
      download.file(url, dest_file)

      warning(glue::glue("Failed to download {url}"))
      return(NULL)
    },

    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @param level Administrative level
    #' @param res Resolution
    #' @return Data frame with provincial emissions
    extract_emissions_from_grid = function(vect, gridded_data, iso2, level, res) {
      # Implementation will depend on the specific format of CEDS netCDF files
      # This is a placeholder that would need to be filled with the actual extraction logic

      # Extract data for each netCDF file
      results <- pbapply::pblapply(gridded_data$nc_files, function(nc_file) {
        # Extract pollutant and year from filename
        filename <- basename(nc_file)
        parts <- strsplit(filename, "_")[[1]]
        pollutant <- parts[1]
        year <- as.numeric(gsub("\\.nc$", "", parts[2]))

        # Open netCDF file
        nc <- ncdf4::nc_open(nc_file)

        # Create raster from netCDF
        r <- terra::rast(nc_file)

        # Extract zonal statistics
        zonal_stats <- terra::extract(r, vect, fun = sum, na.rm = TRUE)

        # Prepare results data frame
        result <- data.frame(
          GID_0 = iso2,
          NAME_0 = countrycode::countrycode(iso2, "iso2c", "country.name"),
          GID_1 = vect$GID_1,
          NAME_1 = vect$NAME_1,
          pollutant = pollutant,
          year = year,
          emission = zonal_stats[, 2]  # Assuming the extracted value is in column 2
        )

        # Close netCDF file
        ncdf4::nc_close(nc)

        return(result)
      }) %>%
        dplyr::bind_rows()

      return(results)
    }
  )
)
