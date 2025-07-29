#' @title CEDSSource
#' @description R6 class for CEDS emissions data
#'
#' @importFrom R6 R6Class
#' @export
CEDSSource <- R6::R6Class(
  "CEDSSource",
  inherit = EmissionsSource,

  public = list(
    #' @description Create a new CEDSSource object
    #' @param version Data version
    #' @param available_years Available years
    initialize = function(version = "v_2024_04_01",
                          available_years = 2000:2022) {
      super$initialize(
        name = "CEDS",
        version = version,
        available_years = available_years,
        base_url = "https://zenodo.org/records/10904361"
      )
    },

    #' @description Download CEDS data
    #' @return Path to extracted data directory
    download_data = function() {
      # Construct URL and file paths
      url <- glue::glue("{self$base_url}/files/CEDS_{self$version}_detailed.zip?download=1")
      file_zip <- file.path(self$cache_dir, glue::glue("CEDS_{self$version}_detailed.zip"))

      # Download and extract
      message("Downloading CEDS data from ", url)
      download.file(url, file_zip)

      message("Extracting zip file...")
      unzip(file_zip, exdir = self$cache_dir)

      # Return path to extracted directory
      return(tools::file_path_sans_ext(file_zip))
    },

    #' @description Process CEDS data
    #' @param data_dir Directory containing CSV files
    #' @param min_year Minimum year to include
    #' @return Data frame with processed emissions data
    process_data = function(data_dir, min_year = NULL) {
      # Use first available year if min_year is NULL
      if (is.null(min_year)) {
        min_year <- min(self$available_years)
      }

      # Find all CSV files
      csv_files <- list.files(data_dir, pattern = ".*\\.csv", full.names = TRUE)
      message("Processing ", length(csv_files), " CSV files...")

      # Parse each file
      parse_file <- function(file) {
        message("Parsing file: ", basename(file))
        readr::read_csv(file) %>%
          tidyr::pivot_longer(cols = -c(country, sector, fuel, units, em),
                             names_to = "year", values_to = "value") %>%
          dplyr::mutate(year = as.numeric(gsub("X", "", year))) %>%
          dplyr::rename(poll = em, iso3 = country) %>%
          dplyr::filter(year >= min_year) %>%
          dplyr::mutate(source = self$name)
      }

      # Combine all data
      data <- lapply(csv_files, parse_file) %>%
        dplyr::bind_rows()

      return(data)
    },

    #' @description Build complete CEDS dataset
    #' @param min_year Minimum year to include
    #' @return Invisibly returns paths to saved files
    build = function(min_year = NULL) {
      # Download data
      extracted_dir <- self$download_data()

      # Process data
      emissions_data <- self$process_data(extracted_dir, min_year)

      # Save data
      results <- self$save_data(emissions_data, by_year = TRUE, by_country = TRUE)

      message("CEDS data build complete!")
      return(invisible(results))
    }
  )
)
