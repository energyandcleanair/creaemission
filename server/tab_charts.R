# Add this near the top of the file, with other reactive declarations
selected_countries <- reactiveVal(NULL)
selected_year <- reactiveVal(NULL)

# Add this observer to update the stored selection when input changes
observeEvent(input$country, {
  selected_countries(input$country)
})

observeEvent(input$year, {
  selected_year(input$year)
})

# Download Handlers ----------------------------------
# Downloadable csv of selected dataset
output$download_csv <- downloadHandler(
  filename = function() {
    paste("emissions.csv", sep = "")
  },
  content = function(file) {
    write.csv(emissions(), file, row.names = FALSE)
  }
)


emissions_raw <- reactive({

  req(input$region_type)
  req(input$country)
  req(input$source)
  topn <- input$topn

  # Years
  single_year_charts <- c("barh")
  multiple_year_charts <- c("area")
  if(input$chart_type %in% single_year_charts){
    years <- input$year
    if(is.null(years)){
      # Probably being set up
      return(NULL)
    }
  } else {
    years <- NULL
  }


  # Adjust isos
  iso3s <- input$country
  if(!is.null(iso3s) & ("all" %in% iso3s)){
    # Get latest year from catalog
    latest_year <- get_latest_year(source=input$source, region_type=input$region_type)

    # Return topn countries
    iso3s_top_n <- get_emissions_national_by_year(year=latest_year, source=input$source) %>%
      group_by(iso3) %>%
      summarise(value=sum(value, na.rm=T)) %>%
      arrange(desc(value)) %>%
      head(topn) %>%
      pull(iso3)

    # Remove all and replace with topn
    iso3s <- iso3s[iso3s!="all"]
    iso3s <- c(iso3s_top_n, iso3s)
    iso3s <- unique(iso3s)
  }



  get_emissions(region_type=input$region_type, iso3s=iso3s, years=years, source=input$source)

})


emissions <- reactive({
  req(input$pollutant)
  req(input$country)
  req(input$chart_type)
  req(emissions_raw())

  e <- emissions_raw()

  e %>%
    filter(poll==input$pollutant) %>%
    filter(("all" %in% input$country & iso3 != "world") | iso3 %in% input$country) %>%
    mutate(sector=clean_sector_name(sector),
           fuel=clean_fuel_name(fuel),
           country=clean_country_name(country))
})


output$selectTopN <- renderUI({
  # Integer output
  numericInput("topn", "Top N:", value=20, min=1, max=100)
})

output$plot <- renderPlotly({

  group_by <- input$group_by
  color_by <- input$color_by
  chart_type <- input$chart_type
  topn <- input$topn

  req(emissions())
  req(group_by)
  req(color_by)
  req(chart_type)

  e <- emissions()

  # Assert that all units start with kt or Gg
  if(!all(grepl("^kt|Gg", e$units))){
    stop("Not all units are in kt")
  }
  unit_suffix <- "kt"
  e <- e %>%
    group_by_at(c(group_by, color_by, "year")) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    ungroup()


  if(chart_type=="barh"){

    e$group <- e %>% pull(group_by)
    e$color <- e %>% pull(color_by)

    # Top N only
    topn_groups <- e %>%
      group_by(group) %>%
      summarise(value_pct=sum(value)/sum(e$value)) %>%
      arrange(desc(value_pct)) %>%
      head(topn) %>%
      pull(group)

    e_plt <- e[e$group%in%topn_groups,] %>%
      filter(value>0)

    e_plt$group <- factor(e_plt$group, levels=rev(topn_groups))

    colourCount = length(unique(e_plt[[color_by]]))
    # getPalette = colorRampPalette(rcrea::pal_crea)
    getPalette = colorRampPalette(brewer.pal(12, "Paired"))

    plt <- e_plt %>%
      ggplot(aes(value, group)) +
       geom_bar(aes(fill=reorder(color, value), text=paste(paste0(color), sprintf("%.2f kt", value), sep="\n")), stat="identity") +
       # geom_text(aes(label=ifelse(value_pct<0.01,"",scales::percent(value_pct, accuracy=.1))), vjust=1, nudge_y = -500, col="white", size=4) +
       rcrea::theme_crea() +
       scale_x_continuous(expand = expansion(mult=c(0, 0.1)),
                           labels = scales::comma_format(suffix=unit_suffix)) +
      scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
       labs(caption=paste0("Source: CREA analysis based on ", input$source, "."),
            y=NULL,
            x=NULL)
  }

  if(chart_type=="area"){


    e$group <- e %>% pull(group_by)
    e$color <- e %>% pull(color_by)

    # Top N only
    topn_groups <- e %>%
      group_by(group) %>%
      summarise(value_pct=sum(value)/sum(e$value)) %>%
      arrange(desc(value_pct)) %>%
      head(topn) %>%
      pull(group)

    e_plt <- e[e$group%in%topn_groups,]


    # Remove those who only have zeros or na
    e_plt <- e_plt %>%
      group_by(group, color) %>%
      filter(sum(value, na.rm=T)>0) %>%
      ungroup()

    e_plt$group <- factor(e_plt$group, levels=topn_groups)

    colourCount = length(unique(e_plt[[color_by]]))
    getPalette = colorRampPalette(brewer.pal(12, "Paired"))


    plt <- e_plt %>%
      mutate(color = reorder(color, value)) %>%
      ungroup() %>%
      ggplot(aes(year, value)) +
      geom_area(aes(fill=color,
                    text=paste(paste0(color)))
                ) +
      rcrea::theme_crea() +
      scale_y_continuous(expand = expansion(mult=c(0, 0.1)),
                         labels = scales::comma_format(suffix=unit_suffix)) +
      scale_fill_manual(values = getPalette(colourCount),
                        name=NULL) +
      labs(caption=paste0("Source: CREA analysis based on ", input$source, "."),
           y=NULL,
           x=NULL) +
      facet_wrap(~group, scales="free")

  }

  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }

  return(ggplotly(plt, tooltip="text") %>% reverse_legend_labels())

})


output$selectYear <- renderUI({
  req(input$chart_type)
  req(input$region_type)
  req(input$source)
  req(input$country)

  years <- get_catalog_years(source=input$source, type=input$region_type)

  if(input$chart_type == 'barh'){
    latest_year <- get_latest_year(source=input$source,
                                  region_type=input$region_type)
    
    # Get previously selected year if it's still available
    prev_selected <- selected_year()
    if(!is.null(prev_selected) && prev_selected %in% years) {
      selected <- prev_selected
    } else {
      selected <- latest_year
    }

    return(selectInput("year", "Year:",
                      multiple=F,
                      choices = rev(years),
                      selected=selected))
  }
  if(input$chart_type == 'area'){
    # No select shown
    return(NULL)
  }
  return(NULL)
})

output$selectCountry <- renderUI({
  req(input$region_type)
  req(input$source)

  multiple = (input$region_type==REGIONTYPE_NATIONAL)

  if(input$region_type==REGIONTYPE_NATIONAL){
    # Get countries from catalog
    countries <- get_catalog_countries(source=input$source, type=input$region_type) %>%
      tibble(iso3 = .) %>%
      mutate(country = iso3_to_country(iso3)) %>%
      filter(iso3!="world", !is.na(country)) %>%
      arrange(country) %>%
      distinct(country, iso3) %>%
      # Transformed to named vector iso=country
      tibble::deframe()
    countries <- c("All"="all", "World"="world", countries)

    # Get previously selected countries that are still available
    prev_selected <- selected_countries()
    if(!is.null(prev_selected)) {
      # Keep only countries that are still available in the new source
      valid_selected <- prev_selected[prev_selected %in% countries]
      if(length(valid_selected) > 0) {
        selected <- valid_selected
      } else {
        selected <- 'all'
      }
    } else {
      selected <- 'all'
    }

    selectInput('country', 'Country', choices = countries, multiple=multiple, selected=selected)
  } else {
    # Get provincial countries from catalog
    countries <- get_catalog_countries(source=input$source, type=input$region_type) %>%
      tibble(iso3 = .) %>%
      mutate(country = iso3_to_country(iso3)) %>%
      distinct(country, iso3) %>%
      tibble::deframe()

    # Get previously selected country if still available
    prev_selected <- selected_countries()
    if(!is.null(prev_selected) && prev_selected %in% countries) {
      selected <- prev_selected
    } else {
      selected <- countries[[1]]
    }

    selectInput('country', 'Country', choices = countries, multiple=multiple, selected=selected)
  }
})



clean_fuel_name <- function(x){
  gsub("_", " ", tolower(x)) %>%
  stringr::str_to_sentence()
}


clean_sector_name <- function(x) {
  # Extract sector_id if it exists at the start (either in brackets or before underscore)
  sector_ids <- str_extract(x, "^\\[([^\\]]+)\\]|^[^_]+")

  # If sector_id is in brackets, extract just the ID part
  sector_ids <- ifelse(!is.na(sector_ids) & str_detect(sector_ids, "^\\["),
                      str_extract(sector_ids, "[^\\[\\]]+"),
                      sector_ids)

  # Clean the name part
  cleaned <- x %>%
    # Remove sector_id from the beginning (either in brackets or with underscore)
    str_remove(paste0("^\\[", sector_ids, "\\]\\s*|^", sector_ids, "_")) %>%
    # Replace "Industrial" with "Industry"
    str_replace_all("Industrial", "Industry") %>%
    # Replace separators with spaces
    str_replace_all("[-_]", " ") %>%
    # Remove "Sector" text
    str_remove(" Sector$") %>%
    # Capitalize first letter
    str_to_sentence() %>%
    # Remove leading/trailing whitespace
    str_trim()

  # Add sector_id in brackets if it exists and is different from the full name
  ifelse(!is.na(sector_ids) & sector_ids != x,
         str_c(cleaned, " [", sector_ids, "]"),
         cleaned)
}


clean_country_name <- function(x){
  # replace NA with International
  x[is.na(x)] <- "International"
  x
}

output$selectPollutant <- renderUI({
  selectInput("pollutant", "Species:", multiple=F, choices = pollutants, selected=pollutants[1])
})

output$selectColorBy <- renderUI({
  req(input$region_type)
  choices <- color_bys

  if(input$region_type=="province"){
     names(choices) <- gsub("Country", "Province", names(choices))
  }

  selectInput("color_by", "Color by:", multiple=F, choices = choices, selected=color_bys[2])
})

output$selectGroupBy <- renderUI({
  req(input$region_type)
  choices <- group_bys

  if(input$region_type=="province"){
    names(choices) <- gsub("Country", "Province", names(group_bys))
  }

  selectInput("group_by", "Group by:", multiple=F, choices = choices, selected=group_bys[1])
})
