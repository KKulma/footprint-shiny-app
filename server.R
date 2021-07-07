server <- function(input, output) {
  
  # map emissions text ----
  
  emissions_text <- eventReactive(input$go, {
    
    # emissions calculation based on airport input ----
    if(input$category == "iata"){
    departure <- stringr::word(input$departure, 1)
    arrival <- stringr::word(input$arrival, 1)
    distance <- airportr::airport_distance(arrival, departure) %>%
      round(2)
    estimate <-
      footprint::airport_footprint(arrival, departure, input$class, input$metric)
    
    # emissions calculation based on city input ----
  } else if(input$category == "city") {
      departure_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(lat)
      departure_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(long)
      arrival_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(lat)
      arrival_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(long)
      distance <- distance_calc(departure_long, departure_lat,
                                arrival_long, arrival_lat) %>%
        round(2)
      estimate <-
        footprint::latlong_footprint(departure_lat, departure_long,
                                     arrival_lat, arrival_long,
                                     input$class, input$metric)
  }
      HTML(
        paste(
          "Estimated Emissions: <br>",
          "<span style='font-size: 160%; color: #3f9323;'><b>",
          estimate,
          "</b></span> kg",
          input$metric
        ))
  })
  
  output$emissions <- renderText({
    emissions_text()
  })
  
  
  # map based on airport input -----
  
  coord_data <- eventReactive(input$go, {
    
    if(input$category == "iata"){
      
    departure <- word(input$departure, 1)
    arrival <- word(input$arrival, 1)
    
    lat_inbound <- airportr::airport_location(arrival) %>%
      pull(Latitude)
    long_inbound <- airportr::airport_location(arrival) %>%
      pull(Longitude)
    lat_outbound <- airportr::airport_location(departure) %>%
      pull(Latitude)
    long_outbound <- airportr::airport_location(departure) %>%
      pull(Longitude)
    
    } else if(input$category == "city") {
      lat_outbound <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(lat)
      long_outbound <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(long)
      lat_inbound <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(lat)
      long_inbound <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(long)
      
    }
    
    coord_data <- gcIntermediate(
      c(long_inbound,
        lat_inbound),
      c(long_outbound,
        lat_outbound),
      n = 100,
      addStartEnd = TRUE,
      sp = TRUE,
      breakAtDateLine = T
    )
  })
  

  output$map <- renderLeaflet({
    coord_data() %>%
      leaflet() %>%
      addTiles(urlTemplate = "https://tile.jawg.io/jawg-dark/{z}/{x}/{y}.png?access-token=qk8Kpqcfoxfg04BI5egRq7tjX8COTiywPrjB2VYrxWQhtWRgnNhmWT3gCEDuobim") %>%
      addPolylines(color = "#6edf49")
  })
  
  # personal carbon budget ----
  
  carbon_budget_text <- eventReactive(input$go, {
    
    # emissions calculation based on airport input ----
    if(input$category == "iata"){
      departure <- stringr::word(input$departure, 1)
      arrival <- stringr::word(input$arrival, 1)
      estimate <-
        footprint::airport_footprint(arrival, departure, input$class, input$metric)
      
      # emissions calculation based on city input ----
    } else if(input$category == "city") {
      departure_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(lat)
      departure_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(long)
      arrival_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(lat)
      arrival_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(long)
      estimate <-
        footprint::latlong_footprint(departure_lat, departure_long,
                                     arrival_lat, arrival_long,
                                    input$class, "co2e")
    }
    
    pct <- scales::percent(round((estimate/1000)/4.8,3))
    
    HTML(
      paste(
        "This trip represents <br>",
        "<span style='font-size: 160%; color: #3f9323;'><b>",
        pct,
        "</b></span><br> of your personal carbon budget for ",
        format(Sys.Date(), "%Y")
      ))
  })
  
  output$carbon_budget <- renderText({
    carbon_budget_text()
  })
  
  # ice melt ----
  
  ice_melt_text <- eventReactive(input$go, {
    
    # emissions calculation based on airport input ----
    if(input$category == "iata"){
      departure <- stringr::word(input$departure, 1)
      arrival <- stringr::word(input$arrival, 1)
      estimate <-
        footprint::airport_footprint(arrival, departure, input$class, input$metric)
      
      # emissions calculation based on city input ----
    } else if(input$category == "city") {
      departure_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(lat)
      departure_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(long)
      arrival_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(lat)
      arrival_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(long)
      estimate <-
        footprint::latlong_footprint(departure_lat, departure_long,
                                     arrival_lat, arrival_long,
                                     input$class, "co2")
    }
    
    ice <- round((estimate/1000)*3,2)
    
    HTML(
      paste(
        "This trip melted about <br>",
        "<span style='font-size: 160%; color: #3f9323;'><b>",
        ice,
        "</b></span><br> square meters of arctic ice <br/>"
      ))
  })
  
  output$ice_melt <- renderText({
    ice_melt_text()
  })
  
  # tree planting ----
  
  tree_text <- eventReactive(input$go, {
    
    # emissions calculation based on airport input ----
    if(input$category == "iata"){
      departure <- stringr::word(input$departure, 1)
      arrival <- stringr::word(input$arrival, 1)
      estimate <-
        footprint::airport_footprint(arrival, departure, input$class, input$metric)
      
      # emissions calculation based on city input ----
    } else if(input$category == "city") {
      departure_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(lat)
      departure_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city1) %>%
        pull(long)
      arrival_lat <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(lat)
      arrival_long <- maps::world.cities %>%
        mutate(name2 = paste(name, ", ", country.etc)) %>%
        filter(name2 == input$city2) %>%
        pull(long)
      estimate <-
        footprint::latlong_footprint(departure_lat, departure_long,
                                     arrival_lat, arrival_long,
                                     input$class, "co2")
    }
    
    trees <- round((estimate/1000)/.06)
    
    HTML(
      paste(
        "It would take <br>",
        "<span style='font-size: 160%; color: #3f9323;'><b>",
        trees,
        "</b></span><br> new trees planted to offset this trip<br/>"
      ))
  })
  
  output$trees <- renderText({
    tree_text()
  })
  
}
