# Create a private environment for caching
.catalog_cache <- new.env(parent = emptyenv())
.catalog_cache$emissions_catalog <- NULL

#' Initialize the global emissions catalog
#' @export
init_emissions_catalog <- function() {
  .catalog_cache$emissions_catalog <- read_emissions_catalog()
}

#' Get the global emissions catalog
#' @export
get_emissions_catalog <- function() {
  if (is.null(.catalog_cache$emissions_catalog)) {
    init_emissions_catalog()
  }
  .catalog_cache$emissions_catalog
}


#' Build a catalog of all emissions data files and their metadata
#' @return A tibble containing the catalog structure
#' @export
build_emissions_catalog <- function() {
  catalog <- purrr::map_dfr(SOURCES, function(source) {
    source_dir <- file.path(PATH_DATA, tolower(source))

    # National data
    national_files <- scan_national_files(source_dir)
    national_catalog <- tibble::tibble(
      source = source,
      type = REGIONTYPE_NATIONAL,
      iso3 = national_files$countries,
      year = list(national_files$years),
      filepath = national_files$country_files,
      yearly_filepath = list(national_files$year_files)
    )

    # Provincial data
    provincial_files <- scan_provincial_files(source_dir)
    provincial_catalog <- tibble::tibble(
      source = source,
      type = REGIONTYPE_PROVINCIAL,
      iso3 = provincial_files$countries,
      year = provincial_files$years,
      filepath = provincial_files$files,
      yearly_filepath = list(NULL)
    )

    dplyr::bind_rows(national_catalog, provincial_catalog)
  })

  saveRDS(catalog, file.path(PATH_DATA, "emissions_catalog.rds"))
  return(catalog)
}

#' Scan national data files for a source
#' @param source_dir Path to the source directory
#' @return List of national data metadata
scan_national_files <- function(source_dir) {
  national_dir <- file.path(source_dir, "national")
  by_year_dir <- file.path(national_dir, "by_year")

  # Scan country files
  country_files <- list.files(national_dir, pattern = "\\.rds$", full.names = TRUE)
  countries <- tools::file_path_sans_ext(basename(country_files))

  # Scan yearly files
  year_files <- list.files(by_year_dir, pattern = "\\.rds$", full.names = TRUE)
  years <- as.integer(stringr::str_extract(basename(year_files), "\\d{4}"))

  list(
    countries = countries,
    years = sort(unique(years)),
    country_files = country_files,
    year_files = year_files
  )
}

#' Scan provincial data files for a source
#' @param source_dir Path to the source directory
#' @return List of provincial data metadata
scan_provincial_files <- function(source_dir) {
  provincial_dir <- file.path(source_dir, "provincial")

  # Scan province files
  province_files <- list.files(provincial_dir, pattern = "\\.rds$", full.names = TRUE)
  countries <- tools::file_path_sans_ext(basename(province_files))

  # Get years for each country by reading files
  years <- lapply(province_files, function(f) {
    if (!file.exists(f)) return(NULL)
    data <- readRDS(f)
    sort(unique(data$year))
  })
  names(years) <- countries

  list(
    countries = countries,
    years = years,
    files = province_files
  )
}

#' Read the emissions catalog
#' @return The emissions catalog list
#' @export
read_emissions_catalog <- function() {
  catalog_file <- "data/emissions_catalog.rds"
  if (!file.exists(catalog_file)) {
    stop("Emissions catalog not found. Run build_emissions_catalog() first.")
  }
  readRDS(catalog_file)
}

#' Get available countries for a source and type
#' @param source Source name (CEDS or EDGAR)
#' @param type Data type (national or provincial)
#' @param catalog Optional catalog dataframe
#' @return Character vector of country codes
#' @export
get_catalog_countries <- function(source, type = REGIONTYPE_NATIONAL, catalog = get_emissions_catalog()) {
  catalog %>%
    dplyr::filter(
      source == toupper(!!source),
      type == !!type
    ) %>%
    pull(iso3)
}

#' Get available years for a source, country, and type
#' @param source Source name (CEDS or EDGAR)
#' @param country Country code (optional for national data)
#' @param type Data type (national or provincial)
#' @param catalog Optional catalog dataframe
#' @return Vector of available years
#' @export
get_catalog_years <- function(source, iso3 = NULL, type = REGIONTYPE_NATIONAL, catalog = get_emissions_catalog()) {
  years <- catalog %>%
    dplyr::filter(
      source == toupper(!!source),
      type == !!type
    )

  if (!is.null(iso3)) {
    years <- years %>%
      dplyr::filter(iso3 == tolower(!!iso3))
  }

  years %>%
    pull(year) %>%
    unlist() %>%
    unique() %>%
    sort()
}

#' Get available pollutants for a source and type
#' @param source Source name (CEDS or EDGAR)
#' @param type Data type (national or provincial)
#' @param catalog Optional catalog dataframe
#' @return Vector of pollutant names
#' @export
get_catalog_pollutants <- function(source, type = REGIONTYPE_NATIONAL, catalog = get_emissions_catalog()) {
  # Read first file to get available pollutants
  first_file <- catalog %>%
    dplyr::filter(
      source == toupper(!!source),
      type == !!type
    ) %>%
    slice(1) %>%
    pull(filepath)

  readRDS(first_file) %>%
    distinct(poll) %>%
    pull(poll)
}

