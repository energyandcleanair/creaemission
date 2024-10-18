download_nc <- function(year, pollutant, dir){

  dir.create(dir, showWarnings = FALSE)
  url <- glue("https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/{pollutant}/{pollutant}-em-anthro_input4MIPs_emissions_CMIP_CEDS-CMIP-2024-07-08_gr_{year}01-{year}12.nc")
  dest_file <- glue("{dir}/{pollutant}_{year}.nc")
  if(file.exists(dest_file)){
    return(dest_file)
  }
  download.file(url, dest_file)
  return(dest_file)
}




extract_provincial_data <- function(year=2022, pollutants=c("NOx", "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "OC", "SO2")){
  dir <- "data/netcdf"
  for(pollutant in pollutants){
    dest_file <- download_nc(year, pollutant, dir)
    # nc <- nc_open(dest_file)
    # data <- ncvar_get(nc, "NOx")
    # Extract the province data
    # ...
  }
}
