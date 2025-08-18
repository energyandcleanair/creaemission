tabPanel("Charts",
         value="charts",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             # Add source selection
             selectInput("source", "Data source:",
                         choices = c('CEDS'='CEDS', 'EDGAR'='EDGAR'),
                         selected='CEDS',
                         multiple=F),

             # Radio button to choose between country and province
             selectInput("region_type", "Region type:",
                           choices = c('Countries'=REGIONTYPE_NATIONAL, 'Provinces'=REGIONTYPE_PROVINCIAL),
                           selected=REGIONTYPE_NATIONAL,
                           multiple=F),
             selectInput("chart_type", "Chart type:", multiple=F, choices = chart_types, selected=chart_types[1]),
             uiOutput("selectYear"),
             uiOutput("selectCountry"),
             uiOutput("selectPollutant"),
             uiOutput("selectColorBy"),
             uiOutput("selectGroupBy"),
             uiOutput("selectTopN"),
             downloadButton(outputId="download_csv", "Download (.csv)", class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             htmlOutput("message", class="msg"),
             tags$style(HTML('.js-plotly-plot .plot-container, .js-plotly-plot .svg-container, .js-plotly-plot .main-svg { overflow: visible !important; }')),
             plotlyOutput("plot", height='calc(100vh - 80px)') %>% withSpinner(color="#8cc9D0")
           )
         )
)

