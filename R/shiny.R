
#' @export
deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  remotes::install_github("energyandcleanair/creahelpers", upgrade = F)
  remotes::install_github("energyandcleanair/rcrea", upgrade = F)

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))
  # # Deploy production
  rsconnect::deployApp(".",
                       # .rscignore doesn't seem to work, we include files manually for now
                       appFiles = c(
                         fs::dir_ls(".", recurse = TRUE, glob = "*.R"),
                         fs::dir_ls("data", recurse = TRUE),
                         fs::dir_ls("server", recurse = TRUE),
                         fs::dir_ls("ui", recurse = TRUE),
                         fs::dir_ls("www", recurse = TRUE)
                       ),
                       appName="ceds",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)

}
