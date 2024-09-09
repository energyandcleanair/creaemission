tabPanel("Charts",
         value="charts",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             # shinyjs::useShinyjs(),


             selectInput("chart_type", "Chart type:", multiple=F, choices = chart_types, selected=chart_types[1]),
             # selectInput("region_type", "Region type:", multiple=F, choices = c('Countries', 'C40 Cities'), selected='Countries'),
             uiOutput("selectYear"),
             # selectInput('country', 'Country', multiple = F, choice = NULL),
             # selectInput('city', 'City', multiple = F, choices = NULL),
             uiOutput("selectCountry"),
             # selectInput('measurement', 'Measurement', multiple = F, choices = c('Absolute', 'Per capita')),
             selectInput("pollutant", "Species:", multiple=F, choices = pollutants, selected=pollutants[1]),
             selectInput("color_by", "Color by:", multiple=F, choices = color_bys, selected=color_bys[2]),
             selectInput("group_by", "Group by:", multiple=F, choices = group_bys, selected=group_bys[1]),



             # selectInput("country", "Country", multiple=F, choices = countries, selected="World"),
             # selectInput("emission_location_id", "Location", multiple=F, choices = unique(results_locations$location_id), selected="wu_1"),
             # actionButton("emission_refresh",
             #              "Refresh",
             #              class="btn-primary"),
             #
             # selectInput("emission_method", "Method", multiple=F, choices = emission_methods, selected=emission_methods[1]),
             # selectInput("emission_indicator", "Indicator", multiple=F, choices = indicators, selected=indicators[1]),
             # # Baseline
             # selectInput("facets", "Facets", multiple=T, choices = facet_choices, selected="crosswind_km"),
             # sliderInput("ws_width", "Wind binning", min=0, max=10, value=1, step=0.5),
             # sliderInput("min_validity_crosswind", "Minimal validity crosswind", min=0, max=1, value=0.8, step=0.1),
             # sliderInput("min_validity_downwind", "Minimal validity downwind", min=0, max=1, value=0.8, step=0.1),
             # uiOutput("selectDateRange"),

             downloadButton(outputId="download_csv", "Download (.csv)", class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             htmlOutput("message", class="msg"),
             plotlyOutput("plot", height='calc(100vh - 80px)') %>% withSpinner(color="#8cc9D0")
           )
         )
)

