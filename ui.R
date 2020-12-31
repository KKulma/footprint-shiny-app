library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(footprint)
library(airportr)
library(geosphere)
library(maps)
library(dqshiny)

# list of airports ----

airports <- airportr::airports %>%
  select(IATA, Name) %>%
  mutate(name2 = paste(IATA, " - ", Name)) %>%
  select(name2) %>% pull()

# list of cities ----

citydata <- maps::world.cities %>%
  mutate(name2 = paste(name, ", ", country.etc)) %>%
  select(name2) %>% pull()


dashboardPage(
  skin = "black",
  dashboardHeader(title = "Flight Footprint Calculator"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      radioButtons(
        inputId = "category",
        label = "Input Type:",
        choices = c("IATA Airport Code" = "iata", 
                    Coordinates = "coord", City = "city"),
        inline = TRUE
      )
    ),
    conditionalPanel(
      "input.category == 'iata'",
      fluidRow(
        column(4,
               autocomplete_input("departure", "Departure Airport:", airports, max_options = 5)),
        column(4,
               autocomplete_input("arrival", "Arrival Airport:", airports, max_options = 5))
      ),
      fluidRow(column(
        4, selectInput("class",
                       "Flight Class",
                       c("Economy", "Economy+", "Business", 
                         "First", "Unknown")
      )),
      column(
        4, selectInput("metric", "Footprint metric*", 
                       c("CO2e - Carbon Dioxide Equivalent" = "co2e",
                         "CO2 - Carbon Dioxide" = "co2", 
                         "CH4 - Methane" = "ch4",
                         "N2O - Nitrous Oxide" = "n2o"))
      ))
    ),
    conditionalPanel(
      "input.category == 'coord'",
      fluidRow(
        column(
          4, numericInput("inlat", "Inbound Latittude", value = 0)
        ),
        column(
          4, numericInput("inlong", "Outbound Longitude", value = 0)
        )),
        fluidRow(
          column(
          4, numericInput("outlat", "Outbound Latittude", value = 0)
        ),
        column(
          4, numericInput("outlong", "Outbound Longitude", value = 0)
        ))),
    conditionalPanel(
      "input.category == 'city'",
      fluidRow(
        column(4,
               autocomplete_input("city1", "Departure City:", citydata, max_options = 5)),
               column(4,
                      autocomplete_input("city2", "Arrival City:", citydata, max_options = 5)
               ))),
    fluidRow(
      column(
        6, actionButton(inputId = "go", label = "Go!"),
      )
    ),
    fluidRow(
             HTML("<div align='center'>"),
             htmlOutput("emissions"),
             HTML("</div>")
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
