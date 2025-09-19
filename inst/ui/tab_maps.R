tabPanel("Maps",
         value="maps",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             uiOutput("map_source_select"),
             uiOutput("map_pollutant_select"),
             uiOutput("map_year_select"),
             uiOutput("map_sector_select"),
             uiOutput("map_country_select"),

             selectInput("map_colormap", "Colormap:",
                        choices = c("Viridis" = "viridis",
                                   "Plasma" = "plasma",
                                   "Inferno" = "inferno",
                                   "Magma" = "magma",
                                   "Cividis" = "cividis"),
                        selected = "viridis"),

            sliderInput("map_opacity", "Opacity:", min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE),

             uiOutput("map_rendering_select"),

             downloadButton(outputId="download_map", "Download (.tif)", class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             class="no-padding",
            leafletOutput("map", height='calc(100vh - 50px)') %>% shinycssloaders::withSpinner(color="#8cc9D0"),
           )
         )
)
