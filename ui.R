# UI function for the Expense Tracker module
expense_tracker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Expense Tracker"),
    numericInput(ns("food"), "Food", value = 0, min = 0),
    numericInput(ns("transport"), "Transport", value = 0, min = 0),
    numericInput(ns("accommodation"), "Accommodation", value = 0, min = 0),
    numericInput(ns("activities"), "Activities", value = 0, min = 0),
    actionButton(ns("submit_expense"), "Submit Expenses")
  )
}

# UI function for Overview Section
overview_section_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Overview"),
      p(strong("Total Budget:"), textOutput(ns("total_budget_display"))),
      p(strong("Remaining Budget:"), textOutput(ns("remaining_budget_display"))),
      p(strong("Biggest Spending Category:"), textOutput(ns("biggest_spending_display"))),
      p(strong("Top Places to Visit:"), textOutput(ns("top_places_display"))),
      p(strong("Weather Forecast:"), textOutput(ns("weather_display")))
    )
  )
}

# Define the UI
ui <- dashboardPage(
  dashboardHeader(
    title = span(
      icon("plane-departure"),
      span("Trip Budget Planner", style = "margin-left: 10px; font-weight: bold") # Bold and larger title
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
            width = 12,
            div(
              style = "display: flex; justify-content: center; align-items: center; height: 80vh;", # Center horizontally and vertically
              box(
                title = tagList(div("Plan Your Trip", style = "font-weight: bold; text-align: center; width: 100%;")),
                width = 8,
                solidHeader = TRUE,
                status = "primary",
                textInput("location", "Your Location:", placeholder = "Enter your current location"),
                textInput("destination", "Travel Destination:", placeholder = "Enter your travel destination"),
                dateInput("arrival_date", "Start Date of Trip:", min = Sys.Date() - 2, value = Sys.Date(), format = "dd-mm-yyyy"),
                dateInput("departure_date", "End Date of Trip:", min = Sys.Date() + 1, value = Sys.Date() + 2, format = "dd-mm-yyyy"),
                numericInput("total_budget", "Total Budget:", value = 1000, min = 1, step = 1),
                actionButton("goToResults", "Go to Results", class = "btn btn-primary")
              )
            )
          )
        )
      ),
      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          column(12,
                 div(style = "text-align: center; position: fixed; bottom: 20px; width: 100%;", actionButton("goToHome", "Go to Home", class = "btn-md btn-primary"))
          )
        ),
        fluidRow(
          column(9,
                 uiOutput("overview_section")
          ),
          column(3,
                 wellPanel(
                   h4("Expense Tracker"),
                   numericInput("food", "Food", value = 0, min = 0),
                   numericInput("transport", "Transport", value = 0, min = 0),
                   numericInput("accommodation", "Accommodation", value = 0, min = 0),
                   numericInput("activities", "Activities", value = 0, min = 0),
                   actionButton("submit_expense", "Submit Expenses")
                 ),
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
        )
      ),
      # Insights Tab
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Travel Insights",
            width = 12,
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