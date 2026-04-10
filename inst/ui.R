library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)

ui <- tagList(
    tags$head(
        # Google Tag Manager
        HTML('<!-- Google Tag Manager -->
<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({\'gtm.start\':
new Date().getTime(),event:\'gtm.js\'});var f=d.getElementsByTagName(s)[0],
j=d.createElement(s),dl=l!=\'dataLayer\'?\'&l=\'+l:\'\';j.async=true;j.src=
\'https://www.googletagmanager.com/gtm.js?id=\'+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,\'script\',\'dataLayer\',\'GTM-M5ZJT39T\');</script>
<!-- End Google Tag Manager -->')
    ),
    # Google Tag Manager (noscript)
    HTML('<!-- Google Tag Manager (noscript) -->
<noscript><iframe src="https://www.googletagmanager.com/ns.html?id=GTM-M5ZJT39T"
height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
<!-- End Google Tag Manager (noscript) -->'),
    navbarPage(
        title=div(img(src="crea_logo.svg", height=44)),
        windowTitle="CREA Emission Portal",
        theme = "theme.css",
        id = "nav-page",
        source(file.path("ui", "tab_charts.R"),  local = TRUE)$value,
        source(file.path("ui", "tab_maps.R"),  local = TRUE)$value,
        source(file.path("ui", "tab_about.R"),  local = TRUE)$value
    )
)
