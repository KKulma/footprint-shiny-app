library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(footprint)
library(airportr)
library(geosphere)

server <- function(input, output) { 
  
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
