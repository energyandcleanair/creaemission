tabPanel("Maps",
         value="maps",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             # shinyjs::useShinyjs(),
             selectInput("map_source", "Data source:",
                         choices = c('CEDS'='CEDS', 'EDGAR'='EDGAR'),
                         selected='CEDS',
                         multiple=F),
             selectInput("map_pollutant", "Pollutant:", multiple=F, choices = c("NOx"="nox"), selected=pollutants[1]),
             selectInput("map_sector", "Sector:", multiple=F, choices = ceds_sectors, selected='total'),
             selectInput("map_palette", "Palette:", multiple=F, choices = map_palettes, selected=map_palettes[1]),
             downloadButton(outputId="download_map", "Download (.tif)", class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             leafletOutput("map", height='calc(100vh - 80px)') %>% withSpinner(color="#8cc9D0"),
           )
         )
)
