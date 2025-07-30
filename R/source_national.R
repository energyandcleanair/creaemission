#' @title SourceNational
#' @description Base class for national emissions data sources
#'
#' @importFrom R6 R6Class
#' @export
SourceNational <- R6::R6Class(
  "SourceNational",

  public = list(
    #' @field data_dir Data directory path
    data_dir = NULL,

    #' @description Initialize the source
    #' @param data_dir Data directory path
    initialize = function(data_dir = NULL) {
      self$data_dir <- data_dir
    },

    #' @description Build national emissions data
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

    #' @description Get emissions data
    #' @param pollutant Pollutant code
    #' @param sector Sector code
    #' @param year Year
    #' @return Data frame with emissions data or NULL if not available
    get = function(pollutant, sector, year) {
      stop("Method must be implemented by subclass")
    },

    #' @description Clear all built data
    #' @return Invisibly returns the number of files removed
    clear = function() {
      stop("Method must be implemented by subclass")
    }
  )
) 