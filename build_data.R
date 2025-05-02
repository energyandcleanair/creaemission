build_data <- function(min_year=2000) {

  version <- "v2024_04_01"
  data_dir <- file.path("data", version)

  # Data downloaded from https://zenodo.org/records/10904361

  url <- "https://zenodo.org/records/10904361/files/CEDS_v_2024_04_01_detailed.zip?download=1"
  dir_tmp <- "data/tmp"
  if (!dir.exists(tmp)) {
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  }
  file_zip <- file.path(dir_tmp, "CEDS_v_2024_04_01_detailed.zip")
  download.file(url, file_zip)
  unzip(file_zip, exdir = dir_tmp)

  csv_files <- list.files(tools::file_path_sans_ext(file_zip), pattern = ".*\\.csv", full.names = TRUE)

  parse_file <- function(file){
    read_csv(file) %>%
    tidyr::pivot_longer(cols = -c(country, sector, fuel, units, em), names_to = "year", values_to = "value") %>%
      mutate(year = as.numeric(gsub("X","", year))) %>%
      rename(poll=em,
             iso=country) %>%
      filter(year >= min_year)
  }


  data <- lapply(csv_files, parse_file) %>%
    bind_rows()

  # split by year and export
  data %>%
    split(.$year) %>%
    walk(~saveRDS(.x, file.path(data_dir, paste0("ceds_emissions_",.x$year[1], ".rds"))))

}

