# Sources
SOURCE_CEDS <- "CEDS"
SOURCE_EDGAR <- "EDGAR"
SOURCES <- c(SOURCE_CEDS, SOURCE_EDGAR)

# Region types
REGIONTYPE_NATIONAL <- "national"
REGIONTYPE_PROVINCIAL <- "provincial"
REGIONTYPES <- c(REGIONTYPE_NATIONAL, REGIONTYPE_PROVINCIAL)

# Pollutants are now derived dynamically from available_data
# No default pollutants constant needed

# File paths
PATH_DATA <- "data"
# PATH_CACHE is now dynamically determined by get_cache_folder()
# This ensures consistency between main code and tests

# Units
UNIT_KT_YEAR <- "kt/year"

# Special values
ISO3_WORLD <- "world"
ISO3_UNATTACHED <- "unattached"

# File patterns
PATTERN_RDS <- "\\.rds$"
PATTERN_YEAR <- "\\d{4}" 