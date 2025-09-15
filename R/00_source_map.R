#' @title SourceMap
#' @description Base class for map sources that handle NetCDF files and raster operations
#'
#' @importFrom R6 R6Class
#' @export
SourceMap <- R6::R6Class(
  "SourceMap",

  public = list(
    #' @field data_dir Data directory path
    data_dir = NULL,

    #' @description Initialize the source
    #' @param data_dir Data directory path
    initialize = function(data_dir = NULL) {
      self$data_dir <- data_dir
    },

    #' @description Build map data (download and process NetCDF files)
    #' @param ... Additional arguments passed to specific implementation
    #' @return Invisibly returns paths to saved files
    build = function(...) {
      stop("Method must be implemented by subclass")
    },

    #' @description List available data combinations
    #' @return Data frame with available pollutant/sector/year combinations
    list_available_data = function() {
      stop("Method must be implemented by subclass")
    },

    #' @description Get map raster
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso3 ISO3 country code
    #' @return Terra raster object or NULL if not available
    get = function(pollutant, sector, year, iso3) {
      stop("Method must be implemented by subclass")
    },

    #' @description Clear all built data
    #' @return Invisibly returns the number of files removed
    clear = function() {
      stop("Method must be implemented by subclass")
    },

    #' @description Crop raster to country boundaries
    #' @param raster Terra raster object
    #' @param iso3 ISO3 country code
    #' @return Cropped and masked raster
    crop_to_country = function(raster, iso3) {
      if (iso3 == "wld") {
        return(raster)
      }
      
      iso2 <- countrycode::countrycode(iso3, "iso3c", "iso2c")
      country_boundaries <- terra::vect(creahelpers::get_adm(level = 0, res = "low", iso2s = iso2))
      
      if (!is.null(country_boundaries)) {
        raster <- terra::crop(raster, country_boundaries)
        raster <- terra::mask(raster, country_boundaries)
      }
      
      return(raster)
    },

    # COG-related methods
    
    #' @description Get COG file path (same directory as NetCDF)
    #' @param pollutant Pollutant code
    #' @param sector Sector code  
    #' @param year Year
    #' @param iso3 ISO3 country code
    #' @return COG file path
    get_cog_path = function(pollutant, sector, year, iso3 = "wld") {
      # COG files go in the same directory as NetCDF files
      filename <- paste0(pollutant, "_", year, "_", sector, "_", iso3, ".tif")
      return(file.path(self$data_dir, filename))
    },
    
    #' @description Get raster using COG if available, fallback to NetCDF
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @param iso3 ISO3 country code
    #' @param prefer_cog Prefer COG over NetCDF if both exist
    #' @return Terra raster object or NULL if not available
    get_cog = function(pollutant, sector, year, iso3 = "wld", prefer_cog = TRUE) {
      cog_path <- self$get_cog_path(pollutant, sector, year, iso3)
      
      if (prefer_cog && file.exists(cog_path)) {
        # Load from COG
        tryCatch({
          return(terra::rast(cog_path))
        }, error = function(e) {
          message(paste0("COG loading failed: ", e$message, ", falling back to NetCDF"))
        })
      }
      
      # Fallback to original NetCDF method
      return(self$get(pollutant, sector, year, iso3))
    }
  )
) 