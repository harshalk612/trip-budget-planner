library(shiny)
library(owmr)

# Define the UI for the Expense Tracker
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

# Define the Server for the Expense Tracker
expense_tracker_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    total_expenses <- reactive({
      input$food + input$transport + input$accommodation + input$activities
    })
    
    return(total_expenses)
  })
}

# UI for Overview Section
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

# Server for Overview Section
overview_section_server <- function(id, total_budget, expense_tracker_data, biggest_spending_category, weather_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive for remaining budget
    remaining_budget <- reactive({
      total_budget() - expense_tracker_data()
    })
    
    # Output for total budget and remaining budget
    output$total_budget_display <- renderText({
      paste("$", total_budget())
    })
    
    output$remaining_budget_display <- renderText({
      remaining <- remaining_budget()
      if (remaining < 0) {
        return("Out of Budget")
      } else {
        return(paste("$", remaining))
      }
    })
    
    # Output for biggest spending category
    output$biggest_spending_display <- renderText({
      biggest_spending_category() %||% "Not Submitted Yet"  # Show default message before clicking
    })
    
    # Placeholder for Top Places to Visit
    output$top_places_display <- renderText({
      "Place A, Place B, Place C"  # Placeholder, can be updated dynamically
    })
    
    # Output for Weather Forecast
    output$weather_display <- renderText({
      weather <- weather_data()
      
      if (!is.null(weather)) {
        # Accessing specific weather details with error handling
        temp <- tryCatch({
          weather$main$temp
        }, error = function(e) {
          cat("Error in accessing temperature: ", e$message, "\n")
          NA
        })
        
        description <- tryCatch({
          weather$weather[[1]]$description
        }, error = function(e) {
          cat("Error in accessing description: ", e$message, "\n")
          NA
        })
        
        if (is.na(temp) | is.na(description)) {
          return("Weather data not available")
        } else {
          return(paste("Temperature:", temp, "°C", "|", "Condition:", description))
        }
      } else {
        return("Weather data not available")
      }
    })
  })
}

# UI function
ui <- fluidPage(
  navbarPage(
    "Travel Budget Planner",
    id = "mainNav",
    tabPanel(
      "Home",
      fluidRow(
        column(
          12,
          h1("Plan Your Trip"),
          textInput(inputId = "location", label = "Your Location:", placeholder = "Enter your current location"),
          textInput(inputId = "destination", label = "Travel Destination:", placeholder = "Enter your travel destination"),
          dateInput(inputId = "arrival_date", label = "Start Date of Trip:", format = "dd-mm-yyyy", value = Sys.Date(), min = Sys.Date() - 2),
          dateInput(inputId = "departure_date", label = "End Date of Trip:", format = "dd-mm-yyyy", value = Sys.Date() + 2, min = Sys.Date() + 1),
          numericInput(inputId = "total_budget", label = "Total Budget:", value = 1000, min = 1, step = 1),
          actionButton("goToResults", "Go to Results")
        )
      )
    ),
    tabPanel(
      "Results",
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
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Total budget input
  total_budget <- reactive({
    input$total_budget
  })
  
  # Create a reactive value to store expenses
  expenses <- reactiveVal(0)  # Initial expense value is 0
  
  # Reactive value to store biggest spending category
  biggest_spending_category <- reactiveVal(NULL)  # Initial state is NULL
  
  # Reactive value to store weather data
  weather_data <- reactiveVal(NULL)  # Initial state is NULL
  
  # Expense Tracker Module
  observeEvent(input$submit_expense, {
    # Calculate total expenses upon button click
    total_expenses <- input$food + input$transport + input$accommodation + input$activities
    expenses(total_expenses)  # Update the stored expenses value
    
    # Determine biggest spending category
    categories <- c("Food" = input$food, 
                    "Transport" = input$transport, 
                    "Accommodation" = input$accommodation, 
                    "Activities" = input$activities)
    
    # Find the category with the highest expense
    max_category <- names(categories)[which.max(categories)]
    
    # Update biggest spending category reactive value
    biggest_spending_category(max_category)
  })
  
  # Overview Section Module
  output$overview_section <- renderUI({
    overview_section_ui("overview_section")
  })
  
  overview_section_server("overview_section", total_budget, expenses, biggest_spending_category, weather_data)
  
  # Observe event for "Go to Results"
  observeEvent(input$goToResults, {
    # Validate "Your Location" field
    if (grepl("[0-9]", input$location)) {
      showModal(
        modalDialog(
          title = "Validation Error",
          "Your Location must not contain numbers.",
          easyClose = TRUE
        )
      )
      return()
    }
    
    # Validate "Travel Destination" field
    if (grepl("[0-9]", input$destination)) {
      showModal(
        modalDialog(
          title = "Validation Error",
          "Destination must not contain numbers.",
          easyClose = TRUE
        )
      )
      return()
    }
    
    # Fetch weather data based on destination using owmr
    weather_data_from_api <- tryCatch({
      owmr::owmr_settings("109d734da4f858e9dca6534a23b73e2b")  # Ensure API key is correctly set
      owmr::get_current(input$destination, units = "metric")
    }, error = function(e) {
      cat("Error fetching weather data:", e$message, "\n")  # Print error message for debugging
      NULL  # Return NULL if there's an error
    })
    
    # Update weather data reactive value
    if (!is.null(weather_data_from_api)) {
      weather_data(weather_data_from_api)
    } else {
      weather_data(NULL)
    }
    
    updateTabsetPanel(session, "mainNav", selected = "Results")
  })
  
  observeEvent(input$goToHome, {
    updateTabsetPanel(session, "mainNav", selected = "Home")
  })
}

# Run the application
shinyApp(ui, server)
