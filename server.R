server <- function(input, output) {
  
  inserted <- c()
  
  observeEvent(input$insertBtn, {
    
    airports <- airportr::airports %>%
      select(IATA, Name) %>%
      mutate(name2 = paste(IATA, " - ", Name)) %>%
      select(name2) %>% pull()
    
    btn <- input$insertBtn
    id <- paste0('txt', btn)
    departure_id <- paste0("departure_", btn)
    arrival_id <- paste0("arrival_", btn)
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        fluidRow(
        column(
          6,
          dqshiny::autocomplete_input(departure_id, "Departure Airport:", airports, max_options = 5, contains = TRUE)
        ),
        column(
          6,
          dqshiny::autocomplete_input(arrival_id, "Arrival Airport:", airports, max_options = 5, contains = TRUE)
        ),
      ),
      id = id)
    )
    inserted <<- c(id, inserted)
  })
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
  })
  
  
  # map emissions text ----
  
  emissions_text <- eventReactive(input$go, {
    
    # emissions calculation based on airport input ----
    departure <- stringr::word(input$departure, 1)
    arrival <- stringr::word(input$arrival, 1)
    
    trip_df <- dplyr::tibble(dep = departure,
                             arr = arrival)
    
    # # FIX not working... - need to remove input if remove button hit
    btn <- input$insertBtn
    if(btn > 0){
      
    for(i in 1:btn){

        dept <- input[[paste0("departure_", i)]]
        arrv <- input[[paste0("arrival_", i)]]
        
        tmp <- dplyr::tibble(dep = stringr::word(dept, 1),
                             arr = stringr::word(arrv, 1))
  
        trip_df <- trip_df %>%
          add_row(tmp)
  
    }
      
      estimate <- trip_df %>%
        rowwise() %>%
        mutate(emissions = footprint::airport_footprint(arr, dep, input$class, input$metric)) %>%
        ungroup() %>%
        summarize(emissions = sum(emissions)) %>%
        pull()

    } else {
    
    estimate <-
      footprint::airport_footprint(arrival, departure, input$class, input$metric)
    
    }
    
    distance <- airportr::airport_distance(arrival, departure) %>%
      round(2)
    
    if(input$trip_type == "roundtrip"){
      estimate <- estimate*2
    } else {estimate}
    
      HTML(
        paste(
          "Estimated Emissions: <br>",
          "<span style='font-size: 160%; color: #3f9323;'><b>",
          estimate,
          "</b></span> kg",
          input$metric, trip_df
        ))
  })
  
  output$emissions <- renderText({
    emissions_text()
  })
  
  
  # map based on airport input -----
  
  coord_data <- eventReactive(input$go, {
    
      
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
    
      departure <- stringr::word(input$departure, 1)
      arrival <- stringr::word(input$arrival, 1)
      estimate <-
        footprint::airport_footprint(arrival, departure, input$class, input$metric)
    
      
      if(input$trip_type == "roundtrip"){
        estimate <- estimate*2
      } else {estimate}
      
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
    
      departure <- stringr::word(input$departure, 1)
      arrival <- stringr::word(input$arrival, 1)
      estimate <-
        footprint::airport_footprint(arrival, departure, input$class, input$metric)
    
      
      if(input$trip_type == "roundtrip"){
        estimate <- estimate*2
      } else {estimate}
      
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
  
      departure <- stringr::word(input$departure, 1)
      arrival <- stringr::word(input$arrival, 1)
      estimate <-
        footprint::airport_footprint(arrival, departure, input$class, input$metric)
      
      
      if(input$trip_type == "roundtrip"){
        estimate <- estimate*2
      } else {estimate}
      
    
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
