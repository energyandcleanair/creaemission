
server <- function(input, output, session) {
  source(file.path("server", "tab_charts.R"),  local = TRUE)$value
  source(file.path("server", "tab_maps.R"),  local = TRUE)$value
  source(file.path("server", "tab_about.R"),  local = TRUE)$value
  
  # Initialize map tab
  init_map_tab(input, output, session)
}

