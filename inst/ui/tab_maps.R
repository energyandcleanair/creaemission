tabPanel("Maps",
         value="maps",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             # shinyjs::useShinyjs(),
             uiOutput("map_source_select"),
             uiOutput("map_pollutant_select"),
             uiOutput("map_year_select"),
             uiOutput("map_sector_select"),
             uiOutput("map_country_select"),
             selectInput("map_palette", "Palette:", 
                        multiple=F, 
                        choices = map_palettes, 
                        selected=map_palettes[1]),
             downloadButton(outputId="download_map", "Download (.tif)", class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             leafletOutput("map", height='calc(100vh - 80px)') %>% withSpinner(color="#8cc9D0"),
           )
         )
)
