#' @title SourceProvincial
#' @description Base class for provincial emissions data sources that use internal map sources
#'
#' @importFrom R6 R6Class
#' @export
SourceProvincial <- R6::R6Class(
  "SourceProvincial",
  public = list(
    #' @field data_dir Data directory path
    data_dir = NULL,
    #' @field map_source Internal map source object
    map_source = NULL,
    #' @description Initialize the source
    #' @param data_dir Data directory path
    #' @param map_source Map source object
    initialize = function(data_dir = NULL, map_source = NULL) {
      self$data_dir <- data_dir
      self$map_source <- map_source
    },

    #' @description Format results to standard format
    #' @param data Data frame to format
    #' @return Formatted data frame
    format_results = function(data) {
      if (is.null(data) || nrow(data) == 0) {
        return(data)
      }

      # Ensure iso3 is lowercase for consistency
      if ("iso3" %in% names(data)) {
        data$iso3 <- tolower(data$iso3)
      }

      # Future formatting rules can be added here:
      # - Standardize pollutant names
      # - Standardize sector names
      # - Standardize units
      # - etc.

      return(data)
    },
    #' @description Build provincial emissions data
    #' @param ... Additional arguments passed to specific implementation
    #' @return Invisibly returns paths to saved files
    build = function(...) {
      stop("Method must be implemented by subclass")
    },
    #' @description List available data combinations
    #' @return Data frame with available pollutant/sector/year/iso2 combinations
    list_available_data = function() {
      stop("Method must be implemented by subclass")
    },
    #' @description Get emissions data
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso2 ISO2 country code
    #' @return Data frame with emissions data or NULL if not available
    get = function(pollutant, sector, year, iso2) {
      stop("Method must be implemented by subclass")
    },
    #' @description Clear all built data
    #' @return Invisibly returns the number of files removed
    clear = function() {
      stop("Method must be implemented by subclass")
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

      # Process each country separately to avoid memory issues
      emissions <- lapply(iso2s, function(iso2) {
        iso3 <- countrycode::countrycode(iso2, "iso2c", "iso3c")
        filepath <- file.path(self$data_dir, paste0(tolower(iso3), ".rds"))

        # Return existing data if available
        if (file.exists(filepath)) {
          message(glue::glue("Loading existing provincial data for {iso2}"))
          return(readRDS(filepath))
        }

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
    #' @description Save provincial data
    #' @param data Provincial emissions data
    #' @return Invisibly returns paths to saved files
    save_provincial_data = function(data) {
      # Create by_year directory if it doesn't exist
      by_year_dir <- file.path(self$data_dir, "by_year")
      if (!dir.exists(by_year_dir)) {
        dir.create(by_year_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Save by country
      country_files <- split(data, data$iso3) %>%
        purrr::map(function(country_data) {
          iso3 <- tolower(country_data$iso3[1])
          file_path <- file.path(self$data_dir, paste0(iso3, ".rds"))
          saveRDS(country_data, file_path)
          return(file_path)
        })

      # Save by year
      year_files <- split(data, data$year) %>%
        purrr::map(function(year_data) {
          year <- year_data$year[1]
          file_path <- file.path(by_year_dir,
                                paste0("provincial_", year, ".rds"))
          saveRDS(year_data, file_path)
          return(file_path)
        })

      return(invisible(list(
        country_files = country_files,
        year_files = year_files
      )))
    },
    #' @description Buffer vector into sea
    #' @param vect Terra vector object
    #' @param id_col ID column name
    #' @param buffer_into_sea_km Buffer distance in km
    #' @param res Resolution ("low" or "high") used for the land mask
    #' @return Buffered terra vector object
    buffer_into_sea = function(vect, id_col, buffer_into_sea_km, res = "low") {
      message(glue::glue("Buffering {id_col} into sea by {buffer_into_sea_km} km"))

      iso3_values <- unique(stats::na.omit(vect$GID_0))
      if (length(iso3_values) != 1) {
        stop("buffer_into_sea() requires exactly one non-missing GID_0 value")
      }

      iso3 <- iso3_values[[1]]

      g_sf <- vect %>%
        sf::st_as_sf() %>%
        sf::st_transform(3857) %>%
        sf::st_make_valid()

      # Get coastal buffer
      g_coast <- cartomisc::regional_seas(
        g_sf,
        group = id_col,
        dist = buffer_into_sea_km * 1000)

      countries_sf <- creahelpers::get_adm(level = 0, res = res) %>%
        terra::vect() %>%
        sf::st_as_sf() %>%
        sf::st_transform(sf::st_crs(g_sf)) %>%
        sf::st_make_valid()

      foreign_land <- suppressWarnings(
        countries_sf %>%
          dplyr::filter(GID_0 != iso3) %>%
          sf::st_crop(sf::st_as_sfc(sf::st_bbox(g_coast)))
      )

      if (nrow(foreign_land) > 0) {
        foreign_land_union <- sf::st_union(foreign_land)
        coast_geometries <- lapply(seq_len(nrow(g_coast)), function(i) {
          coast_geometry <- suppressWarnings(
            sf::st_difference(sf::st_geometry(g_coast)[i], foreign_land_union)
          )

          if (length(coast_geometry) == 0) {
            return(sf::st_geometrycollection())
          }

          coast_geometry[[1]]
        })
        sf::st_geometry(g_coast) <- sf::st_sfc(coast_geometries, crs = sf::st_crs(g_coast))
        g_coast <- g_coast %>%
          dplyr::filter(!sf::st_is_empty(.data$geometry))
      }

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
    #' @description Get the cache path for province boundaries
    #' @param iso2 ISO2 country code
    #' @param level Administrative level (1 for provinces)
    #' @param res Resolution ("low" or "high")
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Cache file path or NULL if no cache directory is configured
    get_boundary_cache_root = function() {
      if (is.null(self$cache_dir) || !nzchar(self$cache_dir)) {
        return(NULL)
      }

      cache_root <- dirname(self$cache_dir)
      boundary_cache_root <- file.path(cache_root, "boundaries")

      if (!dir.exists(boundary_cache_root)) {
        dir.create(boundary_cache_root, recursive = TRUE, showWarnings = FALSE)
      }

      boundary_cache_root
    },
    #' @description Get the cache path for province boundaries
    #' @param iso2 ISO2 country code
    #' @param level Administrative level (1 for provinces)
    #' @param res Resolution ("low" or "high")
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Cache file path or NULL if no cache directory is configured
    get_boundary_cache_path = function(iso2, level = 1, res = "low", buffer_into_sea_km = 20) {
      boundary_cache_root <- self$get_boundary_cache_root()
      if (is.null(boundary_cache_root)) {
        return(NULL)
      }

      boundary_cache_dir <- file.path(boundary_cache_root, tolower(iso2))
      if (!dir.exists(boundary_cache_dir)) {
        dir.create(boundary_cache_dir, recursive = TRUE, showWarnings = FALSE)
      }

      buffer_tag <- gsub("[^0-9A-Za-z]+", "_", as.character(buffer_into_sea_km))
      file.path(
        boundary_cache_dir,
        glue::glue("adm{level}_{tolower(res)}_sea_{buffer_tag}km.gpkg")
      )
    },
    #' @description Get province boundaries
    #' @param iso2 ISO2 country code
    #' @param level Administrative level (1 for provinces)
    #' @param res Resolution ("low" or "high")
    #' @param buffer_into_sea_km Buffer distance into sea in km
    #' @return Terra vector object with provinces
    get_province_boundaries = function(iso2, level = 1, res = "low", buffer_into_sea_km = 20) {
      cache_file <- self$get_boundary_cache_path(iso2, level, res, buffer_into_sea_km)
      if (!is.null(cache_file) && file.exists(cache_file)) {
        message(glue::glue("Loading cached province boundaries for {iso2}"))
        return(terra::vect(cache_file))
      }

      message(glue::glue("Getting province boundaries for {iso2}"))

      # Use creahelpers to get administrative boundaries
      vect <- terra::vect(creahelpers::get_adm(level = level, res = res, iso2s = iso2))

      # Buffer into sea if requested
      if (buffer_into_sea_km > 0) {
        vect <- self$buffer_into_sea(
          vect,
          id_col = glue::glue("GID_{level}"),
          buffer_into_sea_km = buffer_into_sea_km,
          res = res
        )
      }

      if (!is.null(cache_file)) {
        terra::writeVector(vect, cache_file, overwrite = TRUE)
      }

      return(vect)
    },
    #' @description Extract emissions from gridded data for provinces
    #' @param vect Province boundaries as Terra vector
    #' @param gridded_data Gridded data information
    #' @param iso2 ISO2 country code
    #' @return Data frame with provincial emissions
    extract_emissions_from_grid = function(vect, gridded_data, iso2) {
      stop("Method must be implemented by subclass")
    },
    #' @description Download gridded data for provincial analysis
    #' @param years Years to process
    #' @param pollutants Vector of pollutants to process
    #' @return List with gridded directory path and filtered files
    download_gridded_data = function(years = NULL, pollutants = NULL) {
      stop("Method must be implemented by subclass")
    }
  )
)
