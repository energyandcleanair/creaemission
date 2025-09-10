## Shiny Server entrypoint for the real app
## Sources UI/server/global from the package's inst/ directory

app_root <- "/app/inst"
if (!dir.exists(app_root)) {
  stop(sprintf("App root not found: %s", app_root))
}

old_wd <- getwd()
setwd(app_root)
on.exit(setwd(old_wd), add = TRUE)

# Load local package code so `library(creaemission)` resolves
try({
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all("/app", quiet = TRUE)
  }
}, silent = TRUE)

source(file.path(app_root, "global.R"), local = TRUE)
source(file.path(app_root, "ui.R"), local = TRUE)
source(file.path(app_root, "server.R"), local = TRUE)

shiny::shinyApp(ui = ui, server = server)
