#!/usr/bin/env Rscript

## Standalone Shiny entrypoint (no Shiny Server)
## Loads the real app from /app/inst and runs on $PORT

suppressPackageStartupMessages({
  if (requireNamespace("pkgload", quietly = TRUE)) {
    try(pkgload::load_all("/app", quiet = TRUE), silent = TRUE)
  }
  library(shiny)
})

app_root <- "/app/inst"
stopifnot(dir.exists(app_root))

port <- as.integer(Sys.getenv("PORT", "8080"))
host <- "0.0.0.0"

# Run the app from its directory so Shiny serves ./www assets correctly
shiny::runApp(appDir = app_root, host = host, port = port, launch.browser = FALSE)
