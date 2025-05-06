#' @title EDGARSource
#' @description R6 class for EDGAR emissions data
#'
#' @importFrom R6 R6Class
#' @export
EDGARSource <- R6::R6Class(
  "EDGARSource",
  inherit = EmissionsSource,

  public = list(
    #' @description Create a new EDGARSource object
    #' @param version Data version
    #' @param available_years Available years
    initialize = function(version = "v8.1",
                          available_years = 2000:2022) {
      super$initialize(
        name = "EDGAR",
        version = version,
        available_years = available_years,
        # https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v81_FT2022_AP_new/EDGAR_NOx_1970_2022.zip
        base_url = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets"
      )
    },

    #' @description Download EDGAR data
    #' @param pollutants Vector of pollutants to download
    #' @return Path to downloaded data directory
    download_data = function(pollutants = c("CO2", "CH4", "N2O", "SO2", "NOx", "CO", "NMVOC", "NH3", "PM10", "PM2.5", "BC", "OC")) {
      # Create directory for downloaded files
      download_dir <- file.path(self$cache_dir, "edgar_raw")
      if (!dir.exists(download_dir)) {
        dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Download each pollutant
      for (poll in pollutants) {
        message("Downloading EDGAR data for pollutant: ", poll)

        # Construct URL based on pollutant
        # https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v81_FT2022_AP_new/EDGAR_NOx_1970_2022.zip
        url <- glue::glue("{self$base_url}/v81_FT2022_AP_new/EDGAR_{poll}_1970_2022.zip")

        # For SO2 (and maybe other, need to append _v2.zip
        if(poll == "SO2") {
          url <- gsub("\\.zip$", "_v2.zip", url)
        }

        file_zip <- file.path(download_dir, basename(url))

        # Download and extract
        download.file(url, file_zip)
        unzip(file_zip, exdir = download_dir)
      }

      return(download_dir)
    },

    #' @description Process EDGAR data
    #' @param data_dir Directory containing downloaded files
    #' @param min_year Minimum year to include
    #' @return Data frame with processed emissions data
    process_data = function(data_dir, min_year = NULL) {
      # Use first available year if min_year is NULL
      if (is.null(min_year)) {
        min_year <- min(self$available_years)
      }
      print(min_year)

      # Find all CSV files
      xlsx_files <- list.files(data_dir, pattern = ".*\\.xlsx", full.names = TRUE, recursive = TRUE)
      message("Processing ", length(xlsx_files), " XLSX files...")

      # Parse each file (implementation would depend on EDGAR file format)
      parse_file <- function(file) {
        # This is a placeholder - actual implementation would depend on EDGAR format
        message("Parsing file: ", basename(file))
        poll <- gsub(".*EDGAR_([A-Za-z0-9\\.]+)_.*", "\\1", basename(file))
        readxl::read_xlsx(file, sheet = "IPCC 2006", skip=9) %>%
          pivot_longer(cols = starts_with("Y_"), names_to = "year", values_to = "value") %>%
          group_by(
            iso3 = Country_code_A3,
            sector = ipcc_code_2006_for_standard_report_name,
            poll = Substance,
            fuel = fossil_bio,
            year = as.numeric(gsub("Y_", "", year)),
            units = "Gg"
          ) %>%
          dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::filter(year >= min_year) %>%
          dplyr::mutate(source = self$name)
      }

      # Combine all data
      data <- lapply(xlsx_files, parse_file) %>%
        dplyr::bind_rows()

      return(data)
    },

    #' @description Build complete EDGAR dataset
    #' @param min_year Minimum year to include
    #' @param pollutants Vector of pollutants to download
    #' @return Invisibly returns paths to saved files
    build = function(min_year = NULL,
                    pollutants = c("NOx", "SO2")) {
      # Download data
      download_dir <- self$download_data(pollutants)

      # Process data
      emissions_data <- self$process_data(download_dir, min_year)

      # Save data
      results <- self$save_data(emissions_data, by_year = TRUE, by_country = TRUE)

      message("EDGAR data build complete!")
      return(invisible(results))
    }
  )
)
