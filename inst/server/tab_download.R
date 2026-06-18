# Download tab ------------------------------------------------------------
# Lets users export raw emissions data (national or provincial) for any
# combination of sources, countries, pollutants and years as a single CSV.
#
# Performance note: national get(iso3 = NULL) reads ~220 per-country files
# (~70s, >12M rows), so we never load "everything" eagerly. We fetch one
# country at a time (a per-country read is ~0.2s) and:
#   - the live preview loads only a small capped sample (fast, on every change)
#   - the full dataset is materialised only when the user clicks Download.

# Bindings to satisfy linters for dplyr NSE columns
poll <- NULL

# Preview limits (keep the UI responsive)
DL_PREVIEW_COUNTRIES <- 1
DL_PREVIEW_ROWS <- 200

# Combined "available data" across the selected sources for the chosen region
# type. Cheap (cached / sample-based); used to populate the choices.
dl_available <- reactive({
  req(input$dl_region_type, input$dl_sources)

  parts <- lapply(input$dl_sources, function(src) {
    ad <- get_current_source(src, input$dl_region_type)$list_available_data()
    if (is.null(ad) || nrow(ad) == 0) return(NULL)
    ad$data_source <- src
    ad
  })

  dplyr::bind_rows(parts)
})

# All selectable (non-world) iso3 codes for the current region/source set.
dl_all_isos <- reactive({
  ad <- dl_available()
  if (is.null(ad) || nrow(ad) == 0) return(character(0))
  isos <- sort(unique(ad$iso3))
  isos[isos != "world"]
})

# Resolve the current country selection into a concrete vector of iso3 codes.
dl_resolve_isos <- function() {
  isos_sel <- input$dl_countries
  if (is.null(isos_sel)) return(character(0))
  if ("all" %in% isos_sel) dl_all_isos() else setdiff(isos_sel, "all")
}

# Country selector (dynamic, multiselect with Select All / search) ----------
output$dl_selectCountry <- renderUI({
  req(input$dl_region_type, input$dl_sources)
  isos <- dl_all_isos()
  req(length(isos) > 0)

  labels <- creaemission::iso3_to_country(isos)
  choices <- stats::setNames(isos, ifelse(is.na(labels), toupper(isos), labels))
  choices <- c("All countries" = "all", choices)

  # National defaults to the first country (loading all ~220 is opt-in via the
  # "All countries" entry); provincial also defaults to its first country.
  selected <- isos[1]

  shinyWidgets::pickerInput(
    "dl_countries", "Country/Countries:",
    choices = choices, selected = selected, multiple = TRUE,
    options = list(`actions-box` = TRUE, `live-search` = TRUE,
                   `selected-text-format` = "count > 2")
  )
})

# Pollutant selector (dynamic, multiselect with Select All) -----------------
output$dl_selectPollutant <- renderUI({
  ad <- dl_available()
  req(ad, nrow(ad) > 0)

  polls <- sort(unique(ad$pollutant))
  shinyWidgets::pickerInput(
    "dl_pollutants", "Pollutant(s):",
    choices = polls, selected = polls, multiple = TRUE,
    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
  )
})

# Year selector (dynamic, multiselect with Select All) ----------------------
output$dl_selectYear <- renderUI({
  ad <- dl_available()
  req(ad, nrow(ad) > 0)

  years <- sort(unique(ad$year))
  shinyWidgets::pickerInput(
    "dl_years", "Year(s):",
    choices = years, selected = years, multiple = TRUE,
    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4")
  )
})

# Core fetch: loop one country at a time and filter in R. ---------------------
# iso_limit  - only read the first N countries (preview); NULL = all
# row_cap    - stop once this many rows are collected (preview); NULL = no cap
# progress   - show a withProgress bar (used for the full download)
dl_fetch <- function(iso_limit = NULL, row_cap = NULL, progress = FALSE) {
  region_type <- input$dl_region_type
  sources <- input$dl_sources
  polls <- input$dl_pollutants
  years <- as.integer(input$dl_years)

  if (is.null(region_type) || is.null(sources) || length(sources) == 0 ||
      is.null(polls) || is.null(years)) {
    return(NULL)
  }

  iso_vec <- dl_resolve_isos()
  if (length(iso_vec) == 0) return(NULL)
  if (!is.null(iso_limit) && length(iso_vec) > iso_limit) {
    iso_vec <- iso_vec[seq_len(iso_limit)]
  }

  combos <- expand.grid(src = sources, iso = iso_vec, stringsAsFactors = FALSE)
  n <- nrow(combos)
  parts <- vector("list", n)

  fetch_one <- function(src, iso) {
    obj <- get_current_source(src, region_type)
    d <- tryCatch(
      obj$get(pollutant = NULL, sector = NULL, year = NULL, iso3 = iso),
      error = function(e) {
        message(sprintf("Download: get() failed for %s/%s: %s", src, iso, e$message))
        NULL
      }
    )
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d <- d %>% dplyr::filter(poll %in% polls, year %in% years)
    if (nrow(d) == 0) return(NULL)
    d$data_source <- src
    d
  }

  loop_body <- function() {
    for (k in seq_len(n)) {
      if (progress) {
        shiny::incProgress(1 / n, detail = sprintf("%s · %s (%d/%d)",
                                                   combos$src[k], combos$iso[k], k, n))
      }
      parts[[k]] <<- fetch_one(combos$src[k], combos$iso[k])
      if (!is.null(row_cap)) {
        got <- sum(vapply(parts, function(x) if (is.null(x)) 0L else nrow(x), integer(1)))
        if (got >= row_cap) break
      }
    }
  }

  if (progress) {
    shiny::withProgress(message = "Loading emissions data...", value = 0, loop_body())
  } else {
    loop_body()
  }

  out <- dplyr::bind_rows(parts)
  if (nrow(out) == 0) return(out)

  out <- out %>%
    dplyr::relocate(dplyr::any_of(
      c("data_source", "iso3", "region_id", "region_name",
        "poll", "sector", "sector_group", "fuel", "year", "unit", "units", "value")
    ))

  if (!is.null(row_cap)) out <- utils::head(out, row_cap)
  out
}

# Live preview: small, fast sample (first country, capped rows) -------------
dl_preview_data <- reactive({
  req(input$dl_region_type, input$dl_sources,
      input$dl_countries, input$dl_pollutants, input$dl_years)
  dl_fetch(iso_limit = DL_PREVIEW_COUNTRIES, row_cap = DL_PREVIEW_ROWS)
})

# Summary line above the preview (cheap: based on the selection, not the data)
output$dl_summary <- renderUI({
  if (is.null(input$dl_sources) || length(input$dl_sources) == 0) {
    return(HTML("<p>Select at least one source.</p>"))
  }
  req(input$dl_region_type, input$dl_countries, input$dl_pollutants, input$dl_years)

  heavy <- length(dl_resolve_isos()) > 20

  warning <- if (heavy) {
    paste0("<p style='color:#b35a00'>Large selection &mdash; generating the full ",
           "CSV may take up to a minute after you click <b>Download</b>.</p>")
  } else {
    ""
  }

  caption <- sprintf("<p class='text-muted'>Preview below: up to %s rows from the first country.</p>",
                     DL_PREVIEW_ROWS)

  HTML(paste0(warning, caption))
})

# Preview table (small sample only) -----------------------------------------
output$dl_preview <- DT::renderDataTable({
  d <- dl_preview_data()
  req(d, nrow(d) > 0)
  DT::datatable(
    d,
    rownames = FALSE,
    options = list(pageLength = 10, scrollX = TRUE)
  )
})

# Download handler (full dataset, computed on click) ------------------------
output$dl_download_csv <- downloadHandler(
  filename = function() {
    srcs <- paste(tolower(input$dl_sources), collapse = "-")
    sprintf("creaemission_%s_%s_%s.csv",
            input$dl_region_type, srcs, Sys.Date())
  },
  content = function(file) {
    d <- dl_fetch(progress = TRUE)
    if (is.null(d)) d <- data.frame()
    write.csv(d, file, row.names = FALSE)
  }
)
