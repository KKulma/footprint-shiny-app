# list of airports ----

airports <- airportr::airports %>%
  select(IATA, Name) %>%
  mutate(name2 = paste(IATA, " - ", Name)) %>%
  select(name2) %>% pull()

# distance function ----
distance_calc <- function(departure_long, departure_lat, 
                     arrival_long, arrival_lat){

    lon1 = departure_long * pi/180
    lat1 = departure_lat * pi/180
    lon2 = arrival_long * pi/180
    lat2 = arrival_lat * pi/180
    radius = 6373
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
    b = 2 * atan2(sqrt(a), sqrt(1 - a))
    distance = radius * b
}

# begin dashboard ----

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Air Travel Carbon Footprint Calculator"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      radioButtons(
        inputId = "trip_type",
        label = NULL,
        choices = c(
          "Round Trip" = "roundtrip",
          "One Way" = "oneway"
          #Coordinates = "coord"
        ),
        inline = TRUE
      )
    ),
      fluidRow(
        column(
          4,
          dqshiny::autocomplete_input("departure", "Departure Airport:", airports, max_options = 5, contains = TRUE)
        ),
        column(
          4,
          dqshiny::autocomplete_input("arrival", "Arrival Airport:", airports, max_options = 5, contains = TRUE)
        )
      ),
    fluidRow(column(
      6,
      actionButton('insertBtn', 'Add Leg'), 
      actionButton('removeBtn', 'Remove Leg'), 
      tags$div(id = 'placeholder') 
    )),
    fluidRow(column(
      4, selectInput(
        "class",
        "Flight Class",
        c("Economy", "Economy+", "Business",
          "First", "Unknown")
      )
    ),
    column(
      4, selectInput(
        "metric",
        "Footprint metric*",
        c(
          "CO2e - Carbon Dioxide Equivalent" = "co2e",
          "CO2 - Carbon Dioxide" = "co2",
          "CH4 - Methane" = "ch4",
          "N2O - Nitrous Oxide" = "n2o"
        )
      )
    )),
    # conditionalPanel(
    #   "input.category == 'coord'",
    #   fluidRow(column(
    #     4, numericInput("inlat", "Inbound Latittude", value = 0)
    #   ),
    #   column(
    #     4, numericInput("inlong", "Outbound Longitude", value = 0)
    #   )),
    #   fluidRow(column(
    #     4, numericInput("outlat", "Outbound Latittude", value = 0)
    #   ),
    #   column(
    #     4, numericInput("outlong", "Outbound Longitude", value = 0)
    #   ))
    # ),
    fluidRow(column(
      6, actionButton(inputId = "go", label = "Go!"),
    )),
    # float emissions over map ----
    fluidRow(
      column(6,
      shiny::HTML("<div class='floatcontainer'>"),
          shiny::HTML("<div class='box' style='background: black;'>"),
            leaflet::leafletOutput("map"),
          shiny::HTML("</div>"), #box div
          shiny::HTML("<div class='box stack-top' style='background: white;'>"),
            htmlOutput("emissions"),
          shiny::HTML("</div>"), #stack-top div
      shiny::HTML("</div>")), #container div
      column(6,
             p(htmlOutput("carbon_budget")),
             p(htmlOutput("ice_melt")),
             p(htmlOutput("trees")),
             )
    ),
    fluidRow(
      shiny::p("* All estimates include radiative forcing")
    ),
    box(
      title = "Methodology", solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 10,
      h3("Emissions"),
      HTML(
        paste(
          "Emissions are calculated with the <a href='https://github.com/acircleda/footprint' target='_blank'>footprint R package</a>.")),
      h3("Personal Carbon Footprint"),
      HTML("The personal carbon footprint for 2020-2030 ranges from 3.7 to 4.8 metric tons of CO2e per person. This range is dervived from a number of scenarios that limit global warming to 1.5 or 2 degrees Celsius above pre-industrial temperatures. For our calculator's estimate, we divide the metric tons of CO2e from the inputted trip by the upper range of the carbon budget, 4.8.",
           "<br><br>Source: <a href='https://www.iges.or.jp/en/pub/15-degrees-lifestyles-2019/en' target='_blank'>1.5-Degree Lifestyles: Targets and options for reducing lifestyle carbon footprints</a>"),
      h3("Arctic Ice Melt"),
      HTML("This estimate is based on a linear relationship between CO2e and arctice ice melt at a rate of approximately 3 square meters per metric ton of CO2",
           "<br><br>Source: <a href='https://doi.org/10.1126/science.aag2345' target='_blank'>Observed Arctic sea-ice loss directly follows anthropogenic CO2 emission</a>"),
      h3("Tree Planting"),
      HTML("The number of newly-planted trees required to offset a trip specifically refers to the number of urban medium growth coniferous or deciduous tree seedlings grown for 10 years in the United States. It is estimated that one tree in this scenario can sequester .060 metric tons of CO2.",
           "<br><br>Source: <a href='file:///C:/Users/aschmidt/Zotero/storage/2YEFXCII/greenhouse-gases-equivalencies-calculator-calculations-and-references.html' target='_blank'>Greenhouse Gases Equivalencies Calculator - Calculations and References</a>"),
    )
  )
)
