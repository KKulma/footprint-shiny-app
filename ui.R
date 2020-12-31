library(shinydashboard)
library(leaflet)
library(footprint)

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
        6, selectInput("metric", "Footprint metric", c("co2e", "co2", "ch4", "n2o"))
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
    fluidRow(),
    fluidRow(
      leafletOutput("map")
    )
    
  )
)
