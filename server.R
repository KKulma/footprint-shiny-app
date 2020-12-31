library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(footprint)
library(airportr)
library(geosphere)
library(maps)
library(dqshiny)

server <- function(input, output) { 
  
  # list of cities ----
  
  data <- maps::world.cities %>%
    mutate(name2 = paste(name, ", ", country.etc)) %>%
      select(name2) %>% pull()
  
  emissions_text <- eventReactive(input$go, {
    departure <- input$outbound
    arrival <- input$inbound
    dist <- airportr::airport_distance(input$inbound, input$outbound)
    estimate <- footprint::airport_footprint(input$inbound, input$outbound, input$class, input$metric)
    
    HTML(paste("<b>", departure, "</b> to <b>", arrival, "</b>", "Estimated Emissions: <br>",
               "<span style='font-size: 160%; color: #3f9323;'><b>", estimate, "</b></span>", input$metric))
  })
  
  output$emissions <- renderText({
    emissions_text()
  })

  
  # map -----
  
  coord_data <- eventReactive(input$go,{
  
  lat_inbound <- airportr::airport_location(input$inbound) %>%
    pull(Latitude)
  long_inbound <- airportr::airport_location(input$inbound) %>%
    pull(Longitude)
  lat_outbound <- airportr::airport_location(input$outbound) %>%
    pull(Latitude)
  long_outbound <- airportr::airport_location(input$outbound) %>%
    pull(Longitude)
  
  coord_data <- gcIntermediate(c(long_inbound, 
                                            lat_inbound), 
                                          c(long_outbound,
                                            lat_outbound),
                                          n=100, 
                                          addStartEnd=TRUE,
                                          sp=TRUE)
  })
  
  
  
  output$map <- renderLeaflet({
    coord_data() %>% 
    leaflet() %>% 
    addTiles() %>% 
    addPolylines()
  })
  
  }
