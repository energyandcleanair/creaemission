
#' @export
deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))
  # # Deploy production
  rsconnect::deployApp(".",
                       appName="c40_gas",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)

}
