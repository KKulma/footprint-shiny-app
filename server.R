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
  

  # emissions calculation based on airport input ----
  
  emissions_text <- eventReactive(input$go, {
    departure <- word(input$departure,1)
    arrival <- word(input$arrival,1)
    distance <- airportr::airport_distance(arrival, departure) %>% 
      round(2)
    estimate <- footprint::airport_footprint(arrival, departure, input$class, input$metric)
    
    HTML(paste("<b>", departure, "</b> to <b>", arrival, "</b> (", distance, "km) Estimated Emissions: <br>",
               "<span style='font-size: 160%; color: #3f9323;'><b>", estimate, "</b></span>", input$metric))
  })
  
  output$emissions <- renderText({
    emissions_text()
  })

  
  # map based on airport input -----
  
  coord_data <- eventReactive(input$go,{
    
    departure <- word(input$departure,1)
    arrival <- word(input$arrival,1)
  
  lat_inbound <- airportr::airport_location(arrival) %>%
    pull(Latitude)
  long_inbound <- airportr::airport_location(arrival) %>%
    pull(Longitude)
  lat_outbound <- airportr::airport_location(departure) %>%
    pull(Latitude)
  long_outbound <- airportr::airport_location(departure) %>%
    pull(Longitude)
  
  coord_data <- gcIntermediate(c(long_inbound, 
                                            lat_inbound), 
                                          c(long_outbound,
                                            lat_outbound),
                                          n=100, 
                                          addStartEnd=TRUE,
                                          sp=TRUE,
                                          breakAtDateLine = T)
  })
  
  
  
  output$map <- renderLeaflet({
    coord_data() %>% 
    leaflet() %>% 
      addTiles(urlTemplate = "https://tile.jawg.io/jawg-dark/{z}/{x}/{y}.png?access-token=qk8Kpqcfoxfg04BI5egRq7tjX8COTiywPrjB2VYrxWQhtWRgnNhmWT3gCEDuobim") %>% 
      addPolylines(color = "#6edf49")
  })
  
  }
