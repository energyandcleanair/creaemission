#' @title CEDSNational
#' @description R6 class for CEDS national emissions data
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr distinct rename filter bind_rows mutate %>%
#' @importFrom tidyr crossing
#' @importFrom countrycode countrycode
#' @export
CEDSNational <- R6::R6Class(
  "CEDSNational",
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

    #' @description Create a new CEDSNational object
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Data directory path
    initialize = function(version = "v_2024_04_01",
                          available_years = 2000:2022,
                          data_dir = NULL) {
      # Use path resolution if data_dir is not provided
      if (is.null(data_dir)) {
        data_dir <- get_data_path(c("ceds", "national"))
      }

      super$initialize(data_dir = data_dir)
      self$version <- version
      self$available_years <- available_years
      self$base_url <- "https://zenodo.org/records/10904361"
      self$cache_dir <- get_cache_folder("ceds")

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
      data$sector <- map_values(data$sector, CEDS_NATIONAL_SECTOR_MAPPING)

      # Step 2: Apply SECTOR_GROUP_MAPPING to create sector groups
      data$sector_group <- map_values(data$sector, CEDS_NATIONAL_SECTOR_GROUP_MAPPING)

      return(data)
    },

    #' @description Build national emissions data
    #' @param min_year Minimum year to include
    #' @return Invisibly returns paths to saved files
    build = function(min_year = NULL) {
      # Download data
      extracted_dir <- self$download_data()

      # Process data
      emissions_data <- self$process_data(extracted_dir, min_year)

      # Save data in both formats
      results <- self$save_data(emissions_data, by_year = TRUE, by_country = TRUE)

      message("CEDS national data build complete!")
      return(invisible(results))
    },

    #' @description List available data combinations
    #' @param year Optional year filter
    #' @param sector Optional sector filter
    #' @param pollutant Optional pollutant filter
    #' @return Data frame with available pollutant/sector/year combinations
    list_available_data = function(year = NULL, sector = NULL, pollutant = NULL) {
      # Check cache first - if we have cached data and no filters, return it immediately
      if (!is.null(self$available_data_cache) && is.null(pollutant) && is.null(year) && is.null(sector)) {
        return(self$available_data_cache)
      }

      # Check by_year directory
      by_year_dir <- file.path(self$data_dir, "by_year")
      if (!dir.exists(by_year_dir)) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Get all RDS files and extract years from filenames
      rds_files <- list.files(by_year_dir, pattern = "\\.rds$", full.names = TRUE)

      if (length(rds_files) == 0) {
        return(data.frame(
          pollutant = character(),
          sector = character(),
          year = integer(),
          iso3 = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Extract years from filenames (much faster than reading all files)
      available_years <- as.integer(gsub("ceds_emissions_(\\d{4})\\.rds", "\\1", basename(rds_files)))

      # Read only ONE file to get the structure (sectors, pollutants, iso3 are the same across years)
      sample_file <- rds_files[1]

      tryCatch({
        sample_data <- readRDS(sample_file)

        if (nrow(sample_data) > 0) {
          # Extract unique combinations from the sample file
          base_combinations <- sample_data %>%
            dplyr::distinct(poll, sector, iso3) %>%
            dplyr::rename(pollutant = poll)

          # Create all combinations by crossing with available years
          result <- base_combinations %>%
            tidyr::crossing(year = available_years)

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
      message("CEDS list_available_data: Cache cleared")
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
        by_year_file <- file.path(self$data_dir, "by_year", paste0("ceds_emissions_", year, ".rds"))

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

      self$clear_cache()

      message(glue::glue("Cleared {removed_count} CEDS national data files"))
      return(invisible(removed_count))
    },

    #' @description Download CEDS data
    #' @return Path to extracted data directory
    download_data = function() {
      # Construct URL and file paths
      url <- glue::glue("{self$base_url}/files/CEDS_{self$version}_detailed.zip?download=1")
      file_zip <- file.path(self$cache_dir, glue::glue("CEDS_{self$version}_detailed.zip"))

      # Check if zip file already exists
      if (file.exists(file_zip)) {
        message("CEDS zip file already exists, skipping download")
      } else {
        # Download and extract
        message("Downloading CEDS data from ", url)
        download.file(url, file_zip)
      }

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
          dplyr::mutate(source = "CEDS")
      }

      # Combine all data
      data <- lapply(csv_files, parse_file) %>%
        dplyr::bind_rows()

      # Map pollutants
      data <- data %>%
        mutate(poll = map_values(poll, CEDS_POLLUTANTS))

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
            file_path <- file.path(year_dir, paste0("ceds_emissions_", year, ".rds"))
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
                dplyr::mutate(iso3 = "world", source = "CEDS")
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
