library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(leaflet)
library(plotly)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = span(
      icon("plane-departure"),
      span("Trip Budget Planner", style = "margin-left: 10px; font-weight: bold")
    ),
    titleWidth = 270
  ),
  dashboardSidebar(
    minified = TRUE, collapsed = TRUE,
    width = 270,
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Smart Itinerary Planner", tabName = "itinerary", icon = icon("wand-magic-sparkles")),
      menuItem("Hotels", tabName = "hotels", icon = icon("hotel")),
      menuItem("Recommendations", tabName = "recommendations", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            offset = 3,
            box(
              title = "Plan Your Trip",
              solidHeader = TRUE,
              textInput("location", "Your Location:", placeholder = "Enter your location"),
              textInput("destination", "Travel Destination:", placeholder = "Enter your destination"),
              dateInput("arrival_date", "Arrival Date:", value = Sys.Date(), min = Sys.Date()),
              dateInput("departure_date", "Departure Date:", value = Sys.Date() + 1, min = Sys.Date() + 1),
              numericInput("native_budget", "Total Budget (Native Currency):", value = 1000, min = 0),
              # textInput("native_currency", "Native Currency Code (e.g., USD, EUR):", value = "USD"),
              # numericInput("native_amount", "Amount in Native Currency:", value = 1000, min = 0),
              actionButton("goToResults", strong("Go to Results"), class = "btn btn-secondary")
            )
          )
        )
      ),
      tabItem(
        tabName = "results",
        fluidRow(
          column(
            width = 9,
            wellPanel(
              h4("Overview"),
              p(strong("Total Budget:"), textOutput("total_budget_display")),
              p(strong("Total Expenses:"), textOutput("total_expenses_display")),
              p(strong("Remaining Budget:"), textOutput("remaining_budget_display")),
              p(strong("Biggest Spending Category:"), textOutput("biggest_spending_display")),
              p(strong("Weather Forecast:"), textOutput("weather_display"), plotlyOutput("temp_forecast"))
            )
          ),
          column(
            width = 3,
            wellPanel(
              h4("Currency Conversion"),
              p("Native Currency Amount:"),
              textOutput("native_amount_display"),
              p("Converted Amount in Destination Currency:"),
              textOutput("converted_amount_display")
            ),
            wellPanel(
              h4("Recommendations"),
              uiOutput("overview_recommendations")
            )
            )
          ),
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Expense Tracker"),
              sidebarLayout(
                sidebarPanel(
                  textInput("item", "Enter Item Name:", ""),
                  numericInput("budget", "Enter Budget:", value = 0, min = 0),
                  actionButton("submit_expense", "Submit Expense", class = "btn btn-secondary"),
                  actionButton("clear_expenses", "Clear All Expenses", class = "btn btn-danger"),
                  hr(),
                  tableOutput("expense_table")
                ),
                mainPanel(
                  plotOutput("expense_pie"),
                  downloadButton("download", "Download Expenses")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "itinerary",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Generate Itinerary",
              solidHeader = TRUE,
              status = "navy",
              actionButton("generate_itinerary", "Generate Itinerary", class = "btn btn-secondary")
            )
          ),
          column(
            width = 8,
            box(
              title = "Generated Itinerary",
              solidHeader = TRUE,
              status = "info",
              uiOutput("formatted_itinerary")
            )
          )
        )
      ),
      tabItem(
        tabName = "hotels",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Find Hotels",
              solidHeader = TRUE,
              p("Hotels will be searched using your travel destination."),
              actionButton("update_hotels", "Search Hotels", class = "btn btn-secondary")
            )
          ),
          column(
            width = 8,
            leafletOutput("hotel_map", height = 400),
            br(),
            DT::dataTableOutput("hotel_table")
          )
        )
      ),
      tabItem(
        tabName = "recommendations",
        fluidRow(
          
          box(
            title = "Spending Breakdown",
            width = 12,
            plotOutput("spending_bar_chart", height = "400px")
          ),
          box(
            title = "Recommendations",
            width = 12,
            tabsetPanel(
              tabPanel("Places to Visit", uiOutput("cheap_destinations_map")),
              tabPanel("Cheaper Transport", dataTableOutput("cheap_transport_table"))
            )
          )
        )
      )
      )
    ),
  options = list(sidebarExpandOnHover = TRUE)
)
