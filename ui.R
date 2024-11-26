# ui.R

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# Define the UI
ui <- dashboardPage(
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
      menuItem("Insights", tabName = "insights", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12, # Content width
            offset = 3, # Center the content by adding an offset
            allign = 'centre',
            box(
              title = "Plan Your Trip",
              solidHeader = TRUE,
              textInput("location", "Your Location:", placeholder = "Enter your location"),
              textInput("destination", "Travel Destination:", placeholder = "Enter your destination"),
              dateInput("arrival_date", "Arrival Date:", value = Sys.Date()),
              dateInput("departure_date", "Departure Date:", value = Sys.Date() + 1),
              numericInput("total_budget", "Total Budget:", value = 1000, min = 0),
              actionButton("goToResults", "Go to Results", class = "btn btn-primary")
            )
          )
        )
      ),
      # Results Tab (including Expense Tracker inside it)
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
              p(strong("Weather Forecast:"), textOutput("weather_display"))
            )
          ),
          column(
            width = 3,
            wellPanel(
              h4("Recommendations"),
              p("Explore local attractions and activities."),
              p("Book a guided tour or enjoy cultural experiences.")
            ),
            wellPanel(
              h4("Travel Insights"),
              p("Check the weather in your destination."),
              p("Explore visa and entry requirements.")
            )
          )
        ),
        # Integrating Expense Tracker in the Results Tab
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Expense Tracker"),
              sidebarLayout(
                sidebarPanel(
                  textInput("item", "Enter Item Name:", ""),
                  numericInput("budget", "Enter Budget:", value = 0, min = 0),
                  actionButton("submit_expense", "Submit Expense", class = "btn btn-primary"),
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
      # Insights Tab
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Travel Insights",
            solidHeader = TRUE,
            status = "info",
            p("Coming soon: Get detailed travel insights!")
          )
        )
      )
    )
  ),
  title = "Budget Trip Planner",
  options = list(sidebarExpandOnHover = TRUE)
)
