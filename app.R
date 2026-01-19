library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Zambia CDF Projects"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", 
                  "Select Year:", 
                  choices = NULL),
      
      selectInput("column", 
                  "Select Column to Map:", 
                  choices = NULL)
    ),
    
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to make column names nicer
  make_nice_name <- function(name) {
    name %>%
      gsub("_", " ", .) %>% 
      gsub("cdf", "CDF ", .) %>%
      tools::toTitleCase() 
      
  }
  
  # Load data from files
  raw_data_full <- read.csv("data/cdf_data_clean.csv")
  
  # Load GeoJSON
  constits <- st_read("data/zmb_constits.geojson", quiet = TRUE)
  
  # Update year choices with default
  observe({
    if ("year" %in% names(raw_data_full)) {
      years <- sort(unique(raw_data_full$year), decreasing = TRUE)
      updateSelectInput(session, "year", 
                        choices = years,
                        selected = years[1])
    }
  })
  
  # Filter data by selected year
  raw_data <- reactive({
    req(input$year)
    if ("year" %in% names(raw_data_full)) {
      raw_data_full %>% filter(year == input$year)
    } else {
      raw_data_full
    }
  })
  
  # Update column choices based on available numeric columns
  observe({
    req(input$year)
    data <- raw_data()
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    # Remove 'year' from the column choices if it exists
    numeric_cols <- setdiff(numeric_cols, "year")
    
    # Create named vector with nice names
    nice_names <- setNames(numeric_cols, sapply(numeric_cols, make_nice_name)) 
    
    updateSelectInput(session, "column", 
                      choices = nice_names,
                      selected = if("cdf_expenditure" %in% numeric_cols) "cdf_expenditure" else numeric_cols[1])
  })
  
  # Create the map
  output$map <- renderLeaflet({
    req(input$column, input$year)
    
    # Get filtered data
    data <- raw_data()
    
    # Merge with geographic data
    geo_data <- constits %>%
      left_join(data, by = "ecz")
    
    # Get selected column values
    values <- geo_data[[input$column]]
    
    # Create viridis color palette
    pal <- colorNumeric(palette = "viridis", domain = values, na.color = "#808080")
    
    # Get nice column name for display
    nice_col_name <- make_nice_name(input$column)

    # Create labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      geo_data[[2]],
      nice_col_name,
      ifelse(is.na(values), "No data", format(round(values, 0), big.mark = ","))
    ) %>% lapply(htmltools::HTML)
    
    # Render map
    leaflet(geo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~values,
        opacity = 0.7,
        title = nice_col_name,
        position = "bottomright"
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)