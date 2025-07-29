#' @title EmissionsSource
#' @description Base R6 class for emissions data sources
#'
#' @importFrom R6 R6Class
#' @export
EmissionsSource <- R6::R6Class(
  "EmissionsSource",

  public = list(
    #' @field name Source name
    name = NULL,

    #' @field available_years Available years for this data source
    available_years = NULL,

    #' @field base_url Base URL for data downloads
    base_url = NULL,

    #' @field version Current version of the data
    version = NULL,

    #' @field data_dir Directory for storing processed data
    data_dir = NULL,

    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @description Create a new EmissionsSource object
    #' @param name Source name
    #' @param version Data version
    #' @param available_years Available years
    #' @param base_url Base URL for downloads
    #' @param data_dir Directory for processed data
    #' @param cache_dir Directory for temporary files
    initialize = function(name, version, available_years, base_url,
                          data_dir = file.path("data", tolower(name)),
                          cache_dir = "data/cache") {
      self$name <- name
      self$version <- version
      self$available_years <- available_years
      self$base_url <- base_url
      self$data_dir <- data_dir
      self$cache_dir <- cache_dir

      # Create directories if they don't exist
      for (dir in c(self$data_dir, self$cache_dir)) {
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
    },

    #' @description Download data from source
    #' @param ... Additional arguments passed to specific implementation
    download_data = function(...) {
      stop("Method must be implemented by subclass")
    },

    #' @description Process raw data
    #' @param ... Additional arguments passed to specific implementation
    process_data = function(...) {
      stop("Method must be implemented by subclass")
    },

    #' @description Save processed data
    #' @param data Data frame to save
    #' @param by_year Whether to split by year
    #' @param by_country Whether to split by country
    save_data = function(data, by_year = TRUE, by_country = TRUE) {
      results <- list()

      # Save by year if requested
      if (by_year) {
        year_dir <- file.path(self$data_dir, "national", "by_year")
        if (!dir.exists(year_dir)) {
          dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
        }

        results$year_files <- data %>%
          split(.$year) %>%
          purrr::map(function(year_data) {
            year <- year_data$year[1]
            file_path <- file.path(year_dir, paste0(tolower(self$name), "_emissions_", year, ".rds"))
            saveRDS(year_data, file_path)
            return(file_path)
          })
      }

      # Save by country if requested
      if (by_country) {
        country_dir <- file.path(self$data_dir, "national")
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
                dplyr::mutate(iso3 = "world", source = self$name)
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
    },

    #' @description Build complete dataset
    #' @param min_year Minimum year to include
    #' @param ... Additional arguments passed to specific implementation
    build = function(min_year = NULL, ...) {
      stop("Method must be implemented by subclass")
    }
  )
)
