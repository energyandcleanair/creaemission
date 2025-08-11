#' @title EmissionsSourceProvincial
#' @description Base R6 class for provincial emissions data extraction
#'
#' @importFrom R6 R6Class
#' @export
EmissionsSourceProvincial <- R6::R6Class(
  "EmissionsSourceProvincial",

  public = list(
    #' @field source_name Name of the emissions source
    source_name = NULL,

    #' @field version Data version
    version = NULL,

    #' @field available_years Available years for this data source
    available_years = NULL,

    #' @field data_dir Directory for storing processed data
    data_dir = NULL,

    #' @field provincial_data_dir Directory for provincial data
    provincial_data_dir = NULL,



    #' @field cache_dir Directory for temporary files
    cache_dir = NULL,

    #' @description Create a new EmissionsSourceProvincial object
    #' @param source_name Source name (e.g., "CEDS", "EDGAR")
    #' @param version Data version
    #' @param available_years Available years
    #' @param data_dir Directory for processed data
    #' @param cache_dir Directory for temporary files
    initialize = function(source_name,
                          version,
                          available_years) {

      self$source_name <- source_name
      self$version <- version
      self$available_years <- available_years

      self$data_dir <- file.path("data", tolower(source_name))
      self$provincial_data_dir <- file.path(self$data_dir, "provincial")
      self$cache_dir <- get_cache_folder(tolower(source_name))

      # Create directories if they don't exist
      for (dir in c(self$data_dir, self$provincial_data_dir, self$cache_dir)) {
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
    },

    #' @description Download gridded data for provincial analysis
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @return List with directory path and filtered files
    download_gridded_data = function(years = NULL, pollutants = NULL) {
      stop("Method must be implemented by subclass")
    },

    #' @description Get province boundaries
    #' @param iso2 ISO2 country code
    #' @param level Administrative level (1 for provinces)
    #' @param res Resolution ("low" or "high")
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Terra vector object with provinces
    get_province_boundaries = function(iso2, level = 1, res = "low", buffer_into_sea_km = 20) {
      message(glue::glue("Getting province boundaries for {iso2}"))

      # Use creahelpers to get administrative boundaries
      vect <- terra::vect(creahelpers::get_adm(level = level, res = res, iso2s = iso2))

      # Buffer into sea if requested (disabled for now to avoid dplyr issues)
      if (buffer_into_sea_km > 0) {
        message(glue::glue("Skipping sea buffer for {iso2} to avoid dplyr dependency"))
        # vect <- self$buffer_into_sea(vect, id_col = glue::glue("GID_{level}"), buffer_into_sea_km)
      }

      return(vect)
    },

    #' @description Buffer vector data into sea
    #' @param vect Terra vector object
    #' @param id_col ID column name
    #' @param buffer_km Buffer distance in km
    #' @return Buffered vector object
    buffer_into_sea = function(vect, id_col, buffer_km = 20) {
      # Convert to sf
      g_sf <- vect %>%
        sf::st_as_sf() %>%
        sf::st_transform(3857) %>%
        sf::st_make_valid()

      # Get coastal buffer
      g_coast <- cartomisc::regional_seas(
        g_sf,
        group = id_col,
        dist = buffer_km * 1000)

      # Join back and reproject
      g_combined <- dplyr::bind_rows(g_sf, g_coast) %>%
        dplyr::group_by_at(id_col) %>%
        dplyr::summarise() %>%
        sf::st_transform(sf::st_crs(sf::st_as_sf(vect))) %>%
        sf::st_make_valid() %>%
        dplyr::filter(!is.na(!!rlang::sym(id_col))) %>%
        dplyr::left_join(g_sf %>%
                  as.data.frame() %>%
                  dplyr::select(-geometry),
                by = id_col) %>%
        terra::vect() %>%
        terra::makeValid()

      return(g_combined)
    },

    #' @description Extract provincial data from gridded data
    #' @param iso2s ISO2 country codes
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param level Administrative level
    #' @param res Resolution ("low" or "high")
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Data frame with provincial emissions
    extract_provincial_data = function(iso2s,
                                      years = NULL,
                                      pollutants = NULL,
                                      level = 1,
                                      res = "low",
                                      buffer_into_sea_km = 20) {
      # Use all available years if years is NULL
      if (is.null(years)) {
        years <- self$available_years
      }

      # Default pollutants if NULL
      if (is.null(pollutants)) {
        pollutants <- c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2")
      }

      # Note: Directory creation is handled by subclasses in download_gridded_data()
      # Subclasses should create "gridded" directories for processed files
      # and avoid creating duplicate "netcdf" directories

      # Process each country separately to avoid memory issues
      emissions <- lapply(iso2s, function(iso2) {
        iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
        filepath <- file.path(self$provincial_data_dir, paste0(tolower(iso3), ".rds"))

        # Return existing data if available
        # if (file.exists(filepath)) {
        #   message(glue::glue("Loading existing provincial data for {iso2}"))
        #   return(readRDS(filepath))
        # }

        message(glue::glue("Extracting emissions for {iso2}"))

        # Get province boundaries
        vect <- self$get_province_boundaries(iso2, level, res, buffer_into_sea_km)

        # Download and prepare gridded data
        gridded_data <- self$download_gridded_data(years, pollutants)

        # Extract emissions for each province
        result <- self$extract_emissions_from_grid(vect, gridded_data, iso2)

        # Save result
        saveRDS(result, filepath)
        return(result)
      }) %>%
        dplyr::bind_rows()

      return(emissions)
    },

    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @param level Administrative level
    #' @param res Resolution
    #' @return Data frame with provincial emissions
    extract_emissions_from_grid = function(vect, gridded_data, iso2, preserve_sector_codes = FALSE) {
      stop("Method must be implemented by subclass")
    },

    #' @description Save provincial data
    #' @param data Provincial emissions data
    #' @return Invisibly returns paths to saved files
    save_provincial_data = function(data) {
      # Create by_year directory if it doesn't exist
      by_year_dir <- file.path(self$provincial_data_dir, "by_year")
      if (!dir.exists(by_year_dir)) {
        dir.create(by_year_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Save by country
      country_files <- split(data, data$GID_0) %>%
        purrr::map(function(country_data) {
          iso3 <- tolower(country_data$GID_0[1])
          file_path <- file.path(self$provincial_data_dir, paste0(iso3, ".rds"))
          saveRDS(country_data, file_path)
          return(file_path)
        })

      # Save by year
      year_files <- split(data, data$year) %>%
        purrr::map(function(year_data) {
          year <- year_data$year[1]
          file_path <- file.path(by_year_dir,
                                paste0(tolower(self$source_name), "_provincial_", year, ".rds"))
          saveRDS(year_data, file_path)
          return(file_path)
        })

      return(invisible(list(
        country_files = country_files,
        year_files = year_files
      )))
    },

    #' @description Build provincial emissions data
    #' @param iso2s ISO2 country codes to process
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @param level Administrative level
    #' @param res Resolution
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Invisibly returns paths to saved files
    build = function(iso2s,
                    years = NULL,
                    pollutants = NULL,
                    level = 1,
                    res = "low",
                    buffer_into_sea_km = 20) {
      # Extract provincial data
      emissions <- self$extract_provincial_data(
        iso2s = iso2s,
        years = years,
        pollutants = pollutants,
        level = level,
        res = res,
        buffer_into_sea_km = buffer_into_sea_km
      )

      # Save provincial data
      results <- self$save_provincial_data(emissions)

      message(glue::glue("{self$source_name} provincial data build complete!"))
      return(invisible(results))
    },

    #' @description Build map files (TIF) for provincial emissions
    #' @param sectors Vector of sectors to process (NULL for all)
    #' @param pollutants Vector of pollutants to process (NULL for all)
    #' @param years Vector of years to process (NULL for all available)
    #' @param iso2s Vector of ISO2 country codes to process (NULL for all available)
    #' @return Invisibly returns paths to saved map files
    build_maps = function(sectors = NULL,
                         pollutants = NULL,
                         years = NULL,
                         iso2s = NULL) {

      # Get available provincial data combinations
      available_data <- self$get_available_provincial_data()

      # Filter by parameters if provided
      if (!is.null(sectors)) {
        available_data <- available_data[available_data$sector %in% sectors, ]
      }
      if (!is.null(pollutants)) {
        available_data <- available_data[available_data$pollutant %in% pollutants, ]
      }
      if (!is.null(years)) {
        available_data <- available_data[available_data$year %in% years, ]
      }
      if (!is.null(iso2s)) {
        available_data <- available_data[available_data$iso2 %in% iso2s, ]
      }

      if (nrow(available_data) == 0) {
        message("No maps to build based on provided parameters")
        return(invisible(0))
      }

      # Create maps directory
      maps_dir <- file.path(self$data_dir, "maps")
      if (!dir.exists(maps_dir)) {
        dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Build each map
      built_files <- list()
      for (i in 1:nrow(available_data)) {
        row <- available_data[i, ]
        message(glue::glue("Building map for {row$pollutant} {row$sector} {row$year} {row$iso2}"))

        map_file <- self$generate_map(
          pollutant = row$pollutant,
          sector = row$sector,
          year = row$year,
          iso2 = row$iso2,
          save = TRUE
        )

        if (!is.null(map_file)) {
          built_files[[length(built_files) + 1]] <- map_file
        }
      }

      message(glue::glue("Built {length(built_files)} map files"))
      return(invisible(length(built_files)))
    },

    #' @description Get available provincial data combinations
    #' @return Data frame with available provincial data combinations
    get_available_provincial_data = function() {
      # This is a base implementation - subclasses should override
      # to provide actual available combinations
      stop("Method must be implemented by subclass")
    },

    #' @description Generate a map raster from provincial data
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso2 ISO2 country code
    #' @param save Whether to save the map file
    #' @return Terra raster object or file path if saved
    generate_map = function(pollutant,
                           sector,
                           year,
                           iso2,
                           save = FALSE) {
      # This is a base implementation - subclasses should override
      # to provide actual map generation
      stop("Method must be implemented by subclass")
    }
  )
)
