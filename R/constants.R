# Sources
SOURCE_CEDS <- "CEDS"
SOURCE_EDGAR <- "EDGAR"
SOURCES <- c(SOURCE_CEDS, SOURCE_EDGAR)

# Region types
REGIONTYPE_NATIONAL <- "national"
REGIONTYPE_PROVINCIAL <- "provincial"
REGIONTYPES <- c(REGIONTYPE_NATIONAL, REGIONTYPE_PROVINCIAL)

# Default pollutants
POLLUTANTS_DEFAULT <- c(
  "NOx", "BC", "CH4", "CO", "CO2", 
  "N2O", "NH3", "NMVOC", "OC", "SO2"
)

# File paths
PATH_DATA <- "data"
PATH_CACHE <- "cache"

# Units
UNIT_KT_YEAR <- "kt/year"

# Special values
ISO3_WORLD <- "world"
ISO3_UNATTACHED <- "unattached"

# File patterns
PATTERN_RDS <- "\\.rds$"
PATTERN_YEAR <- "\\d{4}" 