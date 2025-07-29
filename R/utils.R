#' Convert ISO3 codes to country names with special cases
#'
#' @param iso3 Vector of ISO3 country codes
#' @return Vector of country names
#' @export
iso3_to_country <- function(iso3) {
  result <- countrycode(iso3, "iso3c", "country.name",
              custom_match = c(
                "GLOBAL" = "Global",
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
