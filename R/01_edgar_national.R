#' @title EDGARNational
#' @description R6 class for EDGAR national emissions data
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr distinct rename filter bind_rows mutate
#' @importFrom tidyr crossing
#' @importFrom magrittr %>%
#' @importFrom countrycode countrycode
#' @export
EDGARNational <- R6::R6Class(
  "EDGARNational",
  inherit = SourceNational,

  public = list(
    #' @field version Data version
    version = NULL,

    #' @field available_years Available years
    available_years = NULL,

    #' @field base_url Base URL for downloads
    base_url = NULL,

    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @field available_data_cache Cached available data combinations
    available_data_cache = NULL,

    #' @description Create a new EDGARNational object
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Data directory path
    initialize = function(version = "v8.1",
                          available_years = 2000:2022,
                          data_dir = NULL) {
      # Use path resolution if data_dir is not provided
      if (is.null(data_dir)) {
        data_dir <- get_data_path(c("edgar", "national"))
      }

      super$initialize(data_dir = data_dir)
      self$version <- version
      self$available_years <- available_years
      self$base_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets"
      self$cache_dir <- get_cache_folder("edgar")

      # Create directories if they don't exist
      for (dir in c(self$data_dir, self$cache_dir)) {
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
    },

    #' @description Build national emissions data
    #' @param min_year Minimum year to include
    #' @return Invisibly returns paths to saved files
    build = function(min_year = NULL) {
      # Download data
      downloaded_dir <- self$download_data()

      # Process data
      emissions_data <- self$process_data(downloaded_dir, min_year)

      # Save data in both formats
      results <- self$save_data(emissions_data, by_year = TRUE, by_country = TRUE)

      message("EDGAR national data build complete!")
      return(invisible(results))
    },

    #' @description List available data combinations
    #' @param year Optional year filter
    #' @param sector Optional sector filter
    #' @param pollutant Optional pollutant filter
    #' @return Data frame with available pollutant/sector/year combinations
    list_available_data = function(year = NULL, sector = NULL, pollutant = NULL) {
      start_time <- Sys.time()
      message("EDGAR list_available_data: Starting...")
      
      # Check cache first - if we have cached data and no filters, return it immediately
      if (!is.null(self$available_data_cache) && is.null(pollutant) && is.null(year) && is.null(sector)) {
        cache_time <- difftime(Sys.time(), start_time, units = "secs")
        message("EDGAR list_available_data: Returning cached result in ", round(cache_time, 3), " seconds")
        return(self$available_data_cache)
      }
      
      # Check by_year directory
      dir_check_start <- Sys.time()
      by_year_dir <- file.path(self$data_dir, "by_year")
      if (!dir.exists(by_year_dir)) {
        message("EDGAR list_available_data: Directory doesn't exist, returning empty")
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }
      dir_check_time <- difftime(Sys.time(), dir_check_start, units = "secs")
      message("EDGAR list_available_data: Directory check took ", round(dir_check_time, 3), " seconds")

      # Get all RDS files and extract years from filenames
      file_list_start <- Sys.time()
      rds_files <- list.files(by_year_dir, pattern = "\\.rds$", full.names = TRUE)
      file_list_time <- difftime(Sys.time(), file_list_start, units = "secs")
      message("EDGAR list_available_data: File listing took ", round(file_list_time, 3), " seconds")

      if (length(rds_files) == 0) {
        message("EDGAR list_available_data: No RDS files found, returning empty")
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Extract years from filenames (much faster than reading all files)
      year_extract_start <- Sys.time()
      available_years <- as.integer(gsub("edgar_emissions_(\\d{4})\\.rds", "\\1", basename(rds_files)))
      year_extract_time <- difftime(Sys.time(), year_extract_start, units = "secs")
      message("EDGAR list_available_data: Year extraction took ", round(year_extract_time, 3), " seconds")
      message("EDGAR list_available_data: Found ", length(available_years), " years: ", paste(range(available_years), collapse = "-"))

      # Read only ONE file to get the structure (sectors, pollutants, iso3 are the same across years)
      file_read_start <- Sys.time()
      sample_file <- rds_files[1]
      message("EDGAR list_available_data: Reading sample file: ", basename(sample_file))
      
      tryCatch({
        sample_data <- readRDS(sample_file)
        file_read_time <- difftime(Sys.time(), file_read_start, units = "secs")
        message("EDGAR list_available_data: File read took ", round(file_read_time, 3), " seconds")
        message("EDGAR list_available_data: Sample data dimensions: ", nrow(sample_data), " x ", ncol(sample_data))
        
        if (nrow(sample_data) > 0) {
          # Extract unique combinations from the sample file
          distinct_start <- Sys.time()
          base_combinations <- sample_data %>%
            dplyr::distinct(poll, sector, iso3) %>%
            dplyr::rename(pollutant = poll) %>%
            dplyr::mutate(iso3 = tolower(iso3))
          distinct_time <- difftime(Sys.time(), distinct_start, units = "secs")
          message("EDGAR list_available_data: Distinct combinations took ", round(distinct_time, 3), " seconds")
          message("EDGAR list_available_data: Base combinations: ", nrow(base_combinations))
          
          # Create all combinations by crossing with available years
          crossing_start <- Sys.time()
          result <- base_combinations %>%
            tidyr::crossing(year = available_years)
          crossing_time <- difftime(Sys.time(), crossing_start, units = "secs")
          message("EDGAR list_available_data: Crossing with years took ", round(crossing_time, 3), " seconds")
          message("EDGAR list_available_data: Final combinations: ", nrow(result))
          
        } else {
          # Fallback to empty data frame
          message("EDGAR list_available_data: Sample file is empty")
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

      return(result)
    },

    #' @description Get emissions data
    #' @param pollutant Pollutant code (can be NULL to get all pollutants)
    #' @param sector Sector code (can be NULL to get all sectors)
    #' @param year Year (can be NULL to get all years)
    #' @param iso3 ISO3 country code (can be NULL to get all countries)
    #' @return Data frame with emissions data or NULL if not available
    get = function(pollutant = NULL, sector = NULL, year = NULL, iso3 = NULL) {
      # Try by_year file first
      if (!is.null(year)) {
        by_year_file <- file.path(self$data_dir, "by_year", paste0("edgar_emissions_", year, ".rds"))

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

      # Try by_country files
      country_dir <- file.path(self$data_dir, "by_country")
      if (dir.exists(country_dir)) {
        # Filter files by iso3 if specified
        if (!is.null(iso3)) {
          country_files <- file.path(country_dir, paste0(tolower(iso3), ".rds"))
          country_files <- country_files[file.exists(country_files)]
        } else {
          country_files <- list.files(country_dir, pattern = "\\.rds$", full.names = TRUE)
        }

        all_data <- list()
        for (file in country_files) {
          data <- readRDS(file)

          # Apply filters only if parameters are not NULL
          filtered_data <- data
          if (!is.null(pollutant)) {
            filtered_data <- filtered_data %>% dplyr::filter(poll %in% !!pollutant)
          }
          if (!is.null(sector)) {
            filtered_data <- filtered_data %>% dplyr::filter(sector %in% !!sector)
          }
          if (!is.null(year)) {
            filtered_data <- filtered_data %>% dplyr::filter(year %in% !!year)
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

      # Clear by_year files
      by_year_dir <- file.path(self$data_dir, "by_year")
      if (dir.exists(by_year_dir)) {
        year_files <- list.files(by_year_dir, pattern = "\\.rds$", full.names = TRUE)
        for (file in year_files) {
          if (file.remove(file)) {
            removed_count <- removed_count + 1
          }
        }
      }

      # Clear by_country files
      country_dir <- file.path(self$data_dir, "by_country")
      if (dir.exists(country_dir)) {
        country_files <- list.files(country_dir, pattern = "\\.rds$", full.names = TRUE)
        for (file in country_files) {
          if (file.remove(file)) {
            removed_count <- removed_count + 1
          }
        }
      }

      message(glue::glue("Cleared {removed_count} EDGAR national data files"))
      return(invisible(removed_count))
    },

    #' @description Download EDGAR data
    #' @param pollutants Vector of pollutants to download
    #' @return Path to downloaded data directory
    download_data = function(pollutants = EDGAR_POLLUTANTS) {
      # Create directory for downloaded files
      download_dir <- file.path(self$cache_dir, "edgar_raw")
      if (!dir.exists(download_dir)) {
        dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Download each pollutant
      for (poll in pollutants) {
        message(glue::glue("Downloading EDGAR data for {poll}"))

        # Construct URL
        url <- glue::glue("{self$base_url}/v81_FT2022_AP_new/EDGAR_{gsub('\\\\.','',poll)}_1970_2022.zip")
        dest_file <- file.path(download_dir, glue::glue("EDGAR_{poll}_1970_2022.zip"))

        # For SO2 need to append _v2.zip
        if(poll == "SO2") {
          url <- gsub("\\.zip$", "_v2.zip", url)
        }

        # Download
        if (file.exists(dest_file)) {
          message(glue::glue("File {dest_file} already exists, skipping download."))
        }else{
          download.file(url, dest_file)
        }

        # Extract
        unzip(dest_file, exdir = download_dir)
      }

      return(download_dir)
    },

    #' @description Process EDGAR data
    #' @param data_dir Directory containing extracted data
    #' @param min_year Minimum year to include
    #' @return Data frame with processed emissions data
    process_data = function(data_dir, min_year = NULL) {
      # Use first available year if min_year is NULL
      if (is.null(min_year)) {
        min_year <- min(self$available_years)
      }


      # Find all CSV files
      xlsx_files <- list.files(data_dir, pattern = ".*\\.xlsx", full.names = TRUE, recursive = TRUE)
      message("Processing ", length(xlsx_files), " XLSX files...")

      # Parse each file (implementation would depend on EDGAR file format)
      parse_file <- function(file) {
        # This is a placeholder - actual implementation would depend on EDGAR format
        message("Parsing file: ", basename(file))
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

      # Convert pollutant names to values using EDGAR_POLLUTANTS mapping
      data <- data %>%
        dplyr::mutate(poll = map_values(poll, EDGAR_POLLUTANTS))

      return(data)
    },

    #' @description Save processed data
    #' @param data Data frame to save
    #' @param by_year Whether to split by year
    #' @param by_country Whether to split by country
    #' @return Invisibly returns paths to saved files
    save_data = function(data, by_year = TRUE, by_country = TRUE) {
      results <- list()

      # Save by year if requested
      if (by_year) {
        year_dir <- file.path(self$data_dir, "by_year")
        if (!dir.exists(year_dir)) {
          dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
        }

        results$year_files <- data %>%
          split(.$year) %>%
          purrr::map(function(year_data) {
            year <- year_data$year[1]
            file_path <- file.path(year_dir, paste0("edgar_emissions_", year, ".rds"))
            saveRDS(year_data, file_path)
            return(file_path)
          })
      }

      # Save by country if requested
      if (by_country) {
        country_dir <- file.path(self$data_dir, "by_country")
        if (!dir.exists(country_dir)) {
          dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)
        }

        # Add world totals if not already present
        if (!"world" %in% unique(data$iso3)) {
          data <- data %>%
            dplyr::bind_rows(
              data %>%
                dplyr::group_by(poll, sector, fuel, units, year) %>%
                dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
                dplyr::mutate(iso3 = "world", source = "EDGAR")
            )
        }

        results$country_files <- data %>%
          split(data$iso3) %>%
          purrr::map(function(country_data) {
            iso3 <- tolower(country_data$iso3[1])
            file_path <- file.path(country_dir, paste0(iso3, ".rds"))
            saveRDS(country_data, file_path)
            return(file_path)
          })
      }

      return(invisible(results))
    }
  )
)
