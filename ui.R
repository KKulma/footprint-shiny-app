shinyUI(
  navbarPage(
    title = "Flight Footprint Calculator",
    theme = shinytheme("sandstone"),
    # theme = shinytheme("litera"),
    # theme = shinytheme("darkly"),
    # theme = shinytheme("flatly"),
    # theme = shinytheme("spacelab"),
    # theme = shinytheme("slate"),
    tabPanel(
      "Map",
      div(class = "outer"),
      # tags$head(
      #   includeCSS("www/litera.css")
      # ),
      
      
      fluidRow(
        radioButtons(
          inputId = "category",
          label = "Input Type:",
          choices = c(
            "IATA Airport Code" = "iata",
            Coordinates = "coord",
            City = "city"
          ),
          inline = TRUE
        )
      ),
      conditionalPanel("input.category == 'iata'",
                       fluidRow(
                         column(
                           4,
                           dqshiny::autocomplete_input("departure", "Departure Airport:", airports, max_options = 5)
                         ),
                         column(
                           4,
                           dqshiny::autocomplete_input("arrival", "Arrival Airport:", airports, max_options = 5)
                         )
                       )),
      conditionalPanel(
        "input.category == 'coord'",
        fluidRow(column(
          4, numericInput("inlat", "Inbound Latittude", value = 0)
        ),
        column(
          4, numericInput("inlong", "Outbound Longitude", value = 0)
        )),
        fluidRow(column(
          4, numericInput("outlat", "Outbound Latittude", value = 0)
        ),
        column(
          4, numericInput("outlong", "Outbound Longitude", value = 0)
        ))
      ),
      conditionalPanel("input.category == 'city'",
                       fluidRow(
                         column(
                           4,
                           dqshiny::autocomplete_input("city1", "Departure City:", citydata, max_options = 5)
                         ),
                         column(
                           4,
                           dqshiny::autocomplete_input("city2", "Arrival City:", citydata, max_options = 5)
                         )
                       )),
      fluidRow(column(
        6, actionButton(inputId = "go", label = "Go!"),
      )),
      fluidRow(leaflet::leafletOutput("map")),
      
      absolutePanel(
        id = "hist_panel",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 300,
        left = "auto",
        right = 40,
        bottom = "auto",
        width = "27%",
        height = "auto",
        h3("A nice header"),
        h4(tags$em("Even nicer sub-header")),
        selectInput(
          "class",
          "Flight Class",
          c("Economy", "Economy+", "Business",
            "First", "Unknown")
        ),
        selectInput(
          "metric",
          "Footprint metric*",
          c(
            "CO2e - Carbon Dioxide Equivalent" = "co2e",
            "CO2 - Carbon Dioxide" = "co2",
            "CH4 - Methane" = "ch4",
            "N2O - Nitrous Oxide" = "n2o"
          )
        ),
        fluidRow(
          shiny::HTML("<div align='center'>"),
          htmlOutput("emissions"),
          shiny::HTML("</div>")
        )
      ),
      fluidRow(column(
        6,
        shiny::p("* All estimates include radiative forcing")
      ))
    ),
    tabPanel("About",
             fluidRow())
  )
)
