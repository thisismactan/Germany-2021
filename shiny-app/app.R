library(shiny)
library(shinythemes)
library(leaflet)
library(leafem)
library(readr)
library(sf)
library(sp)
library(tidyverse)

# Shapefiles
state_shp <- read_rds("data/state_shp.rds")
const_shp <- read_rds("data/const_shp.rds")
shp_list <- list("State" = state_shp, "Constituency" = const_shp)

# Vectors/lists that might need to be called later
state_names <- state_shp$name_1
const_names <- const_shp$mouseover_label

const_key <- const_shp %>%
  as.data.frame() %>%
  dplyr::select(const_id = WKR_NR, constituency, state = state_name) %>%
  as_tibble() %>%
  arrange(state, constituency)

# Coordinates for zooming
state_coords <- read_csv("data/state_coords.csv")
const_coords <- read_csv("data/constituency_coords.csv")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    ## Some light CSS for table formatting
    tags$style(HTML("
                    table {
                      border-spacing: 2px;
                    }
                    
                    th, td {
                      padding: 3px;
                    }
                    "))
  ),
  
  # Actual interface
  navbarPage("The Election StatSheet Germany 2021 forecast",
    # The map page
    tabPanel(
      "Map",
      sidebarLayout(
        mainPanel = mainPanel(
          leafletOutput("map", width = 1200, height = 800)
        ),
        sidebarPanel = sidebarPanel(
          # Map geography level
          HTML("<h2>Map options</h2>"),
          HTML("<h3>Geography</h3>"),
          radioButtons(inputId = "map_geography", label = NULL, choices = list("State", "Constituency")),
          br(),
          
          # Zoom to a constituency
          HTML("<h3>Go to...</h3>"),
          inputPanel(selectInput(inputId = "state_select", label = "State", choices = c("Choose a state", state_names),
                                 selected = "Choose a state"),
                     tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                     conditionalPanel(condition = "input.state_select !== 'Choose a state' & input.map_geography == 'Constituency'",
                                      uiOutput("const_menu")),
                     tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                     
                     # Separate buttons for state and constituency level
                     conditionalPanel(condition = "input.map_geography == 'State' & input.state_select !== 'Choose a state'",
                                      actionButton("go_state", "Go!")),
                     conditionalPanel(condition = "input.map_geography == 'Constituency' & input.const_select !== 'Choose a constituency'",
                                      actionButton("go_const", "Go!"))
                     )
        ),
        position = "right"
        
      )
    )
  )
  
  
)


server <- function(input, output) {
  # THE MAP
  ## Allow user to choose geography (state or constituency)
  map_geography <- reactive({
    shp_list[[input$map_geography]]
  })
  
  ## Base map
  output$map <- renderLeaflet({
    leaflet(state_shp) %>%
      addTiles() %>%
      addMouseCoordinates() %>%
      setView(lng = 9, lat = 51.1, zoom = 6)
  })
  
  ## Add appropriate geography polygons based on user choice
  observe({
    leafletProxy("map", data = map_geography()) %>%
      clearShapes() %>%
      addPolygons(color = "white", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color, fillOpacity = ~alpha, label = ~mouseover_label,
                  popup = ~popup_label, highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE, opacity = 1))
  })
  
  ## The go-to section of the sidebar
  output$const_menu <- renderUI({
    selectInput("const_select", label = "Constituency", 
                choices = c("Choose a constituency", const_key %>% filter(state == input$state_select) %>% pull(constituency)))
  })
  
  ## The zoom function
  ### For states
  state_center <- reactive({
    state_coords %>% filter(state == input$state_select)
  })
  
  observeEvent(
    input$go_state,
    handlerExpr = {
      leafletProxy("map", data = map_geography()) %>%
        flyTo(lng = state_center()$lng, lat = state_center()$lat, zoom = state_center()$zoom)
    }
  )
  
  ### For constituencies
  const_center <- reactive({
    const_coords %>% filter(constituency == input$const_select)
  })
  
  observeEvent(
    input$go_const,
    handlerExpr = {
      leafletProxy("map", data = map_geography()) %>%
        flyTo(lng = const_center()$lng, lat = const_center()$lat, zoom = const_center()$zoom)
    }
    
  )
}

shinyApp(ui = ui, server = server)