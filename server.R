
server <- function(input, output, session) {
    source(file.path("server", "tab_charts.R"),  local = TRUE)$value
}
