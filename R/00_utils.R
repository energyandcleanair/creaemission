#' Convert ISO3 codes to country names with special cases
#'
#' @param iso3 Vector of ISO3 country codes
#' @return Vector of country names
#' @export
iso3_to_country <- function(iso3) {
  result <- countrycode(iso3, "iso3c", "country.name",
              custom_match = c(
                "GLOBAL" = "International",
                "SEA" = "International",
                "AIR" = "International",
                "SRB (KOSOVO)" = "Kosovo",  # Kosovo's unofficial ISO3 code
                "WORLD" = "World"
              ))

  # Return the original value if countrycode returns NA
  ifelse(is.na(result), iso3, result)
}

#' Clean fuel names by replacing underscores with spaces and capitalizing
#'
#' @param x Vector of fuel names
#' @return Vector of cleaned fuel names
#' @export
clean_fuel_name <- function(x){
  gsub("_", " ", tolower(x)) %>%
  stringr::str_to_sentence()
}

#' Clean sector names by extracting sector IDs and formatting
#'
#' @param x Vector of sector names
#' @return Vector of cleaned sector names
#' @export
clean_sector_name <- function(x) {
  # Extract sector_id if it exists at the start (either in brackets or before underscore)
  sector_ids <- stringr::str_extract(x, "^\\[([^\\]]+)\\]|^[^_]+")

  # If sector_id is in brackets, extract just the ID part
  sector_ids <- ifelse(!is.na(sector_ids) & stringr::str_detect(sector_ids, "^\\["),
                      stringr::str_extract(sector_ids, "[^\\[\\]]+"),
                      sector_ids)

  # Clean the name part
  cleaned <- x %>%
    # Remove sector_id from the beginning (either in brackets or with underscore)
    stringr::str_remove(paste0("^\\[", sector_ids, "\\]\\s*|^", sector_ids, "_")) %>%
    # Replace "Industrial" with "Industry"
    stringr::str_replace_all("Industrial", "Industry") %>%
    # Replace separators with spaces
    stringr::str_replace_all("[-_]", " ") %>%
    # Remove "Sector" text
    stringr::str_remove(" Sector$") %>%
    # Capitalize first letter
    stringr::str_to_sentence() %>%
    # Remove leading/trailing whitespace
    stringr::str_trim()

  # Add sector_id in brackets if it exists and is different from the full name
  ifelse(!is.na(sector_ids) & sector_ids != x,
         stringr::str_c(cleaned, " [", sector_ids, "]"),
         cleaned)
}

#' Clean country names by replacing NA with "International"
#'
#' @param x Vector of country names
#' @return Vector of cleaned country names
#' @export
clean_country_name <- function(x){
  # replace NA with International
  x[is.na(x)] <- "International"
  x
}

#' Get the project root directory
#' @return Path to the project root directory
get_project_root <- function() {
  # Try to find the project root by looking for key files
  current_dir <- getwd()

  # Check if we're in the inst directory
  if (basename(current_dir) == "inst") {
    return(dirname(current_dir))
  }

  # Check if we're in the project root (has data/ and R/ directories)
  if (dir.exists("data") && dir.exists("R")) {
    return(current_dir)
  }

  # Check if we're in a subdirectory of the project
  parent_dir <- dirname(current_dir)
  if (dir.exists(file.path(parent_dir, "data")) && dir.exists(file.path(parent_dir, "R"))) {
    return(parent_dir)
  }

  # If we can't find it, assume we're in the project root
  return(current_dir)
}

#' Get the data directory path
#' @param subdir Optional subdirectory within data
#' @return Path to the data directory
get_data_path <- function(subdir = NULL) {
  project_root <- get_project_root()
  data_path <- file.path(project_root, "data")

  if (!is.null(subdir)) {
    if (is.character(subdir) && length(subdir) > 0) {
      # Handle vector of subdirectories
      for (dir in subdir) {
        data_path <- file.path(data_path, dir)
      }
    }
  }

  return(data_path)
}

#' Get the R directory path
#' @return Path to the R directory
get_r_path <- function() {
  project_root <- get_project_root()
  return(file.path(project_root, "R"))
}

#' Map values using a named mapping
#'
#' @param values Vector of values to convert
#' @param mapping Named vector or list providing the mapping (names -> values)
#' @return Vector of converted values
#' @export
map_values <- function(values, mapping) {
  sapply(values, function(v) {
    if (v %in% names(mapping)) {
      mapping[[v]]
    } else {
      v
    }
  })
}
