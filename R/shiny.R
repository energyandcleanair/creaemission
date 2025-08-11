
#' @export
deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('rsconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  remotes::install_github("energyandcleanair/creahelpers", upgrade = F)
  remotes::install_github("energyandcleanair/rcrea", upgrade = F)

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))


  # We need to install the package from github so that
  # ShinyApps can reproduce the environment
  remotes::install_github("energyandcleanair/creaemission@feat/edgar", upgrade = T)
  remotes::install_version('curl', version = '6.2.3')

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # Create symlink to data folder
  create_data_symlink()

  # Deploy production - now using the inst/ folder structure
  rsconnect::deployApp("inst",
                       appFiles = c(
                        file.path(gsub("inst/", "", fs::dir_ls("inst", recurse = TRUE))),
                        file.path(fs::dir_ls("data", recurse = TRUE))
                       ),
                       appName="emission4",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)

}

create_data_symlink <- function() {
  # Create a symlink inst/data to data
  data_path <- file.path(getwd(), "data")
  symlink_path <- file.path(getwd(), "inst", "data")

  # Remove any existing symlink or folder with the same name
  if (file.exists(symlink_path) || dir.exists(symlink_path)) {
    unlink(symlink_path, recursive = TRUE)
  }

  # Create the symlink (macOS / Linux)
  system2("ln", c("-s", shQuote(data_path), shQuote(symlink_path)))
}
