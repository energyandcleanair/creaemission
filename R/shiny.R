
#' @export
deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('rsconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  remotes::install_github("energyandcleanair/creahelpers", upgrade = F)
  remotes::install_github("energyandcleanair/rcrea", upgrade = F)

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  # remotes::install_version('curl', version = '6.2.3')
  remotes::install_github("energyandcleanair/creaemission@feat/edgar", upgrade = T)

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # Deploy production - now using the inst/ folder structure
  rsconnect::deployApp("inst",
                       # Include all necessary files
                       appFiles = c(
                       #   # Shiny app files
                         file.path(gsub("inst/", "", fs::dir_ls("inst", recurse = TRUE))),
                       #   # Data folder (needed by the app)
                         file.path("..",fs::dir_ls("data", recurse = TRUE))
                       #   # Package files that might be needed
                       #   # fs::dir_ls("R", recurse = TRUE, glob = "*.R"),
                       #   # "DESCRIPTION",
                       #   # "NAMESPACE"
                       ),
                       appName="emission2",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)

}
