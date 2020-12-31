library(shinydashboard)
library(leaflet)
library(footprint)
library(shiny)

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Flight Footprint"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      radioButtons(
        inputId = "category",
        label = "Input Type:",
        choices = c("IATA Airport Code" = "iata", Coordinates = "coord"),
        inline = TRUE
      )
    ),
    conditionalPanel(
      "input.category == 'iata'",
      fluidRow(
        column(
        6,
        selectInput("outbound", "Departure Airport", c("LHR", "TYS", "LAX"))
      ),
      column(
        6, selectInput("inbound", "Arrival Airport", c("LHR", "TYS", "LAX"))
      )),
      
      fluidRow(column(
        6, selectInput("class",
                       "Flight Class",
                       sort(
                         c("Unknown", "Economy", "Economy+", "Business", "First")
                       ))
      ),
      column(
        6, selectInput("metric", "Footprint metric*", 
                       c("CO2e - Carbon Dioxide Equivalent" = "co2e",
                         "CO2 - Carbon Dioxide" = "co2", 
                         "CH4 - Methane" = "ch4",
                         "N2O - Nitrous Oxide" = "n2o"))
      ))
    ),
    conditionalPanel(
      "input.category == 'coord'",
      fluidRow(
        column(6, numericInput("inlat", "Inbound Latittude", value = 0)),
        column(6, numericInput("inlong", "Inbound Longitude", value = 0)),
        fluidRow(
          column(
          6, numericInput("outlat", "Outbound Latittude", value = 0)
        ),
        column(
          6, numericInput("outlong", "Outbound Longitude", value = 0)
        ))
      )
    ),
    fluidRow(
      column(
        6, actionButton(inputId = "go", label = "Go!"),
      )
    ),
    fluidRow(
      column(6,
             htmlOutput("distance"),
             HTML("<br/>")),
    ),
    fluidRow(
      leafletOutput("map")
    ),
    fluidRow(
      column(
        6,
        p("* All estimates include radiative forcing")
      )
    
  )
))
