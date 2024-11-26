# Load necessary libraries
library(shiny)
library(ggplot2)

# Define the Server
server <- function(input, output, session) {
  # Reactive value to store expenses
  expenses <- reactiveVal(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
  
  # Reactive value to store weather data
  weather_data <- reactiveVal(NULL)
  
  # Classification logic for items
  classify_item <- function(item) {
    keywords <- list(
      Food = c("pizza", "burger", "groceries", "coffee"),
      Accommodation = c("hotel", "hostel", "stay", "rent"),
      Travel = c("flight", "taxi", "bus", "train"),
      Luxury = c("watch", "jewelry", "vacation", "designer")
    )
    for (category in names(keywords)) {
      if (any(grepl(paste(keywords[[category]], collapse = "|"), tolower(item)))) {
        return(category)
      }
    }
    return("Other")
  }
  
  # Submit new expense
  observeEvent(input$submit_expense, {
    if (input$item == "" || input$budget <= 0) {
      showNotification("Please provide a valid item and budget.", type = "error")
      return()
    }
    category <- classify_item(input$item)
    new_expense <- data.frame(
      Item = input$item,
      Category = category,
      Amount = input$budget,
      stringsAsFactors = FALSE
    )
    updated_expenses <- rbind(expenses(), new_expense)
    expenses(updated_expenses)
  })
  
  # Clear all expenses
  observeEvent(input$clear_expenses, {
    expenses(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
    showNotification("All expenses cleared!", type = "message")
  })
  
  # Reactive for total expenses
  total_expenses <- reactive({
    sum(expenses()$Amount, na.rm = TRUE)
  })
  
  # Reactive for remaining budget
  remaining_budget <- reactive({
    input$total_budget - total_expenses()
  })
  
  # Reactive for biggest spending category
  biggest_spending_category <- reactive({
    expense_data <- expenses()
    if (nrow(expense_data) > 0) {
      category_totals <- aggregate(Amount ~ Category, data = expense_data, sum)
      category_totals$Category[which.max(category_totals$Amount)]
    } else {
      "None"
    }
  })
  
  # Handle "Go to Results" button
  observeEvent(input$goToResults, {
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
    weather_data_from_api <- tryCatch({
      owmr::owmr_settings("109d734da4f858e9dca6534a23b73e2b") # Replace with your OpenWeatherMap API key
      owmr::get_current(input$destination, units = "metric")
    }, error = function(e) {
      NULL
    })
    weather_data(weather_data_from_api)
    updateTabItems(session, "sidebarMenu", selected = "results")
  })
  
  # Overview Section Outputs
  output$overview_section <- renderUI({
    overview_section_ui("overview_section")
  })
  
  # Overview Section Outputs
  # Display Total Budget
  output$total_budget_display <- renderText({
    if (is.null(input$total_budget) || input$total_budget <= 0) {
      "No Budget Set"
    } else {
      paste("$", formatC(input$total_budget, format = "f", digits = 0, big.mark = ","))
    }
  })
  
  
  # Display Total Expenses
  output$total_expenses_display <- renderText({
    total_exp <- sum(expenses()$Amount, na.rm = TRUE)
    paste("$", formatC(total_exp, format = "f", digits = 0, big.mark = ","))
  })
  
  
  # Display Remaining Budget
  output$remaining_budget_display <- renderText({
    remaining <- input$total_budget - sum(expenses()$Amount, na.rm = TRUE)
    if (remaining < 0) {
      "Over Budget!"
    } else {
      paste("$", formatC(remaining, format = "f", digits = 0, big.mark = ","))
    }
  })
  
  
  # Display Biggest Spending Category
  output$biggest_spending_display <- renderText({
    expense_data <- expenses()
    if (nrow(expense_data) > 0) {
      category_totals <- aggregate(Amount ~ Category, data = expense_data, sum)
      category_totals$Category[which.max(category_totals$Amount)]
    } else {
      "No Expenses Yet"
    }
  })
  
  # Display Weather Forecast
  output$weather_display <- renderText({
    dest <- input$destination
    if (is.null(dest) || dest == "") {
      return("No destination set.")
    }
    weather <- weather_data()
    if (!is.null(weather)) {
      temp <- tryCatch({ weather$main$temp }, error = function(e) { NA })
      description <- tryCatch({ weather$weather[[1]]$description }, error = function(e) { NA })
      if (!is.na(temp)) {
        paste(temp, "°C")
      } else {
        "Weather data unavailable"
      }
    } else {
      "Weather data unavailable"
    }
  })
  
  
  # Render Expense Table
  output$expense_table <- renderTable({
    expenses()
  })
  
  # Render Pie Chart
  output$expense_pie <- renderPlot({
    expense_data <- expenses()
    if (nrow(expense_data) > 0) {
      expense_summary <- aggregate(Amount ~ Category, data = expense_data, sum)
      ggplot(expense_summary, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = "Expense Distribution")
    }
  })
  
  # Download Expenses
  output$download <- downloadHandler(
    filename = function() {
      paste("expenses-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(expenses(), file, row.names = FALSE)
    }
  )
}
