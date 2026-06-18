tabPanel("Download",
         value = "download",

         div(style = "margin-top: 30px;",
         # Titles in a shared row so they sit on the same baseline above each panel
         fluidRow(
           column(width = 3, h3("Data selection", style = "margin-top: 0;")),
           column(width = 9, h3("Data preview", style = "margin-top: 0;"))
         ),
         sidebarLayout(
           sidebarPanel(
             width = 3,
             # National vs Provincial
             selectInput("dl_region_type", "Region type:",
                         choices = c('Countries (national)' = creaemission::REGIONTYPE_NATIONAL,
                                     'Provinces (provincial)' = creaemission::REGIONTYPE_PROVINCIAL),
                         selected = creaemission::REGIONTYPE_NATIONAL,
                         multiple = FALSE),

             # Source(s) - multiselect
             shinyWidgets::pickerInput(
               "dl_sources", "Source(s):",
               choices = c('CEDS' = 'CEDS', 'EDGAR' = 'EDGAR'),
               selected = c('CEDS', 'EDGAR'),
               multiple = TRUE,
               options = list(`actions-box` = TRUE)
             ),

             # Country/Countries (dynamic) - multiselect with select-all
             uiOutput("dl_selectCountry"),

             # Pollutant(s) (dynamic) - multiselect with select-all
             uiOutput("dl_selectPollutant"),

             # Year(s) (dynamic) - multiselect with select-all
             uiOutput("dl_selectYear"),

             downloadButton(outputId = "dl_download_csv", "Download (.csv)", class = "btn-primary")
           ),
           mainPanel(
             width = 9,
             htmlOutput("dl_summary", class = "msg"),
             DT::dataTableOutput("dl_preview") %>%
               shinycssloaders::withSpinner(color = "#8cc9D0")
           )
         )
         )
)
