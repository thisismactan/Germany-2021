library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(leafem)
library(readr)
library(sf)
library(sp)
library(tidyverse)
library(scales)
library(extrafont)
library(ggiraph)
library(Hmisc)

# Party colors/labels
party_colors = c("afd" = "#00A0E2", "cdu" = "black", "fdp" = "#FEB900", "gruene" = "#19A229", 
                 "linke" = "#BE3075", "other" = "gray", "spd" = "red")

party_names = c("afd" = "Alternative for Germany (AfD)",
                "cdu" = "Union parties (CDU/CSU)",
                "fdp" = "Free Democratic Party (FDP)",
                "gruene" = "Alliance 90/The Greens (Grüne)",
                "linke" = "The Left (Linke)",
                "other" = "Other",
                "spd" = "Social Democratic Party (SPD)")

party_abbr = c("afd" = "AfD", "cdu" = "Union", "fdp" = "FDP", "gruene" = "Grüne", "linke" = "Linke",
               "other" = "Other", "spd" = "SPD")

party_order = c("linke", "gruene", "spd", "fdp", "cdu", "afd")

# Shapefiles
state_shp <- read_rds("data/state_shp.rds")
const_shp <- read_rds("data/const_shp.rds")
shp_list <- list("State" = state_shp, "Constituency" = const_shp)

# Simulation results and timelines
state_sims <- read_csv("data/state_sims.csv") %>%
  mutate(party = ordered(party, levels = party_order))

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

# The app
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
    ),
    
    # Seat/vote projections
    tabPanel(
      "Forecast",
      sidebarLayout(
        ## Main panel: display graphs
        mainPanel = mainPanel(ggiraphOutput("forecast_graph", width = "1200px", height = "800px")),
        
        ## Sidebar panel: choose between projected vote and projected seats, current and over time, possibly filter by state
        sidebarPanel = sidebarPanel(tags$h3("Graph settings"),
                                    radioButtons("graph_type", label = "Graph", choices = c("Current", "Over time")),
                                    pickerInput("state_filter", label = "Select states", choices = state_names, selected = state_names, 
                                                multiple = TRUE,
                                                options = pickerOptions(
                                                  actionsBox = TRUE, 
                                                  selectAllText = "Nationwide",
                                                  deselectAllText = "Deselect all",
                                                  selectedTextFormat = "count > 3")),
                                    actionButton("apply_state_filter", "Apply filters"),
                                    conditionalPanel(condition = "input.graph_type == 'Current'"),
                                    conditionalPanel(condition = "input.graph_type == 'Over time'",
                                                     sliderInput("date_range_polls", "Date range", min = as.Date("2021-01-01"), 
                                                                 max = as.Date("2021-09-26"), value = as.Date(c("2021-01-01", "2021-09-26"))
                                                     )
                                    )
        ),
        position = "right")
    ),
    
    # National polling
    tabPanel("National polling")
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
      addPolylines(opacity = 1, color = "white", weight = 4) %>%
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
  
  # THE FORECAST
  state_sim_subset <- eventReactive(
    input$apply_state_filter,
    valueExpr = {
      state_sims %>%
        filter(state %in% input$state_filter) %>%
        group_by(sim_id, party) %>%
        summarise(total_seats = sum(total_seats)) %>%
        ungroup()
    },
    ignoreNULL = FALSE
  )
  
  state_subset <- eventReactive(
    input$apply_state_filter,
    valueExpr = {
      input$state_filter
    },
    ignoreNULL = FALSE
  )
  
  output$forecast_graph <- renderggiraph({
    # First case: if all states are selected
    if(length(state_subset()) == 16) {
      girafe(ggobj = state_sim_subset() %>%
               ggplot(aes(x = total_seats, y = ..count.. / 10000, fill = party)) +
               geom_vline(data = state_sim_subset() %>% group_by(party) %>% summarise(avg_seats = median(total_seats)),
                          aes(xintercept = avg_seats, col = party), size = 1, show.legend = FALSE) +
               facet_wrap(~party, labeller = labeller(party = party_names), scales = "free_x", nrow = 2) +
               geom_bar(show.legend = FALSE) +
               scale_x_continuous(breaks = 50 * (0:7), 
                                  limits = c(-1, 50 * ceiling((state_sim_subset() %>% pull(total_seats) %>% max()) / 50))) +
               scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_names) +
               scale_colour_manual(name = "Party", values = party_colors, labels = party_names) +
               theme(text = element_text(family = "Lato")) +
               labs(title = "Current projected seats", x = "Seats in Bundestag", y = "Probability",
                    subtitle = "Nationwide")
      )
      # Second case: if more than 3 states are selected
    } else if(length(state_subset()) > 3) {
      girafe(ggobj = state_sim_subset() %>%
               ggplot(aes(x = total_seats, y = ..count.. / 10000, fill = party)) +
               geom_vline(data = state_sim_subset() %>% group_by(party) %>% summarise(avg_seats = median(total_seats)),
                          aes(xintercept = avg_seats, col = party), size = 1, show.legend = FALSE) +
               facet_wrap(~party, labeller = labeller(party = party_names), scales = "free_x", nrow = 2) +
               geom_bar(show.legend = FALSE) +
               scale_x_continuous(breaks = 50 * (0:7), limits = c(-1, 50 * ceiling((state_sim_subset() %>% pull(total_seats) %>% max()) / 50))) +
               scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_names) +
               scale_colour_manual(name = "Party", values = party_colors, labels = party_names) +
               theme(text = element_text(family = "Lato")) +
               labs(title = "Current projected seats", x = "Seats in Bundestag", y = "Probability",
                    subtitle = paste0(paste(head(state_subset(), 3), collapse = ", "), ", and ", length(state_subset()) - 3, " more"))
      )
      # Third case: if between 0 and 3 states are selected, list all of them
    } else if(length(state_subset()) > 0) {
      girafe(ggobj = state_sim_subset() %>%
               ggplot(aes(x = total_seats, y = ..count.. / 10000, fill = party)) +
               geom_vline(data = state_sim_subset() %>% group_by(party) %>% summarise(avg_seats = median(total_seats)),
                          aes(xintercept = avg_seats, col = party), size = 1, show.legend = FALSE) +
               facet_wrap(~party, labeller = labeller(party = party_names), scales = "free_x", nrow = 2) +
               geom_bar(show.legend = FALSE) +
               scale_x_continuous(breaks = 50 * (0:7), limits = c(-1, 50 * ceiling((state_sim_subset() %>% pull(total_seats) %>% max()) / 50))) +
               scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
               scale_fill_manual(name = "Party", values = party_colors, labels = party_names) +
               scale_colour_manual(name = "Party", values = party_colors, labels = party_names) +
               theme(text = element_text(family = "Lato")) +
               labs(title = "Current projected seats", x = "Seats in Bundestag", y = "Probability",
                    subtitle = paste0(paste(head(state_subset(), length(state_subset()) - 1), collapse = ", "), ", and ", 
                                      tail(state_subset(), 1))
               )
      )
    }
  })
}

shinyApp(ui = ui, server = server)