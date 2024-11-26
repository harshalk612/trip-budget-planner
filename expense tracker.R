library(shiny)
library(ggplot2)

# Persistent data storage file
file_path <- "expense_data.csv"

# Define UI
ui <- fluidPage(
  titlePanel("Expense Tracker"),
  sidebarLayout(
    sidebarPanel(
      textInput("item", "Enter Item Name", ""),
      numericInput("budget", "Enter Budget", value = NULL, min = 0),
      actionButton("submit", "Submit Expense"),
      actionButton("clear", "Clear Expenses"),
      hr(),
      tableOutput("expenseTable")
    ),
    mainPanel(
      plotOutput("expensePlot"),
      div(style = "position: absolute; bottom: 10px; left: 10px;",
          downloadButton("download", "Download CSV"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store expenses
  expenseData <- reactiveVal(data.frame(
    Item = character(),
    Budget = numeric(),
    Category = character(),
    stringsAsFactors = FALSE
  ))
  
  # Load saved data if available
  if (file.exists(file_path)) {
    saved_data <- read.csv(file_path, stringsAsFactors = FALSE)
    showModal(modalDialog(
      title = "Load Previous Expenses",
      "You have previously saved expenses. Do you want to load them?",
      footer = tagList(
        modalButton("No"),
        actionButton("loadSaved", "Yes, Load Expenses")
      )
    ))
    
    observeEvent(input$loadSaved, {
      expenseData(saved_data)
      removeModal()
      showNotification("Previously saved expenses loaded successfully.")
    })
  }
  
  # Classification logic
  classifyItem <- function(item) {
    keywords <- list(
      Food = c("pizza", "burger", "groceries", "coffee"),
      Accommodation = c("rent", "hotel", "hostel"),
      Travel = c("flight", "train", "taxi", "bus"),
      Luxury = c("watch", "jewelry", "vacation")
    )
    for (category in names(keywords)) {
      if (any(grepl(paste(keywords[[category]], collapse = "|"), tolower(item)))) {
        return(category)
      }
    }
    return("Other") # Default category if no match
  }
  
  # Error handling
  observeEvent(input$submit, {
    if (is.null(input$item) || input$item == "") {
      showNotification("Item name cannot be empty!", type = "error")
      return()
    }
    if (is.null(input$budget) || input$budget <= 0) {
      showNotification("Budget must be a positive number!", type = "error")
      return()
    }
    
    # Classify item
    category <- classifyItem(input$item)
    
    # Add to expense data
    newExpense <- data.frame(
      Item = input$item,
      Budget = input$budget,
      Category = category,
      stringsAsFactors = FALSE
    )
    updatedData <- rbind(expenseData(), newExpense)
    expenseData(updatedData)
    
    # Save data to CSV immediately
    write.csv(updatedData, file_path, row.names = FALSE)
    
    # Show confirmation
    showNotification(paste("Expense added under category:", category))
  })
  
  # Clear expenses
  observeEvent(input$clear, {
    showModal(modalDialog(
      title = "Clear Expenses",
      "Are you sure you want to clear all expenses?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmClear", "Yes, Clear All")
      )
    ))
  })
  
  observeEvent(input$confirmClear, {
    expenseData(data.frame(
      Item = character(),
      Budget = numeric(),
      Category = character(),
      stringsAsFactors = FALSE
    ))
    write.csv(expenseData(), file_path, row.names = FALSE)
    removeModal()
    showNotification("All expenses cleared!", type = "message")
  })
  
  # Render table of expenses
  output$expenseTable <- renderTable({
    expenseData()
  })
  
  # Render pie chart of expenses
  output$expensePlot <- renderPlot({
    data <- expenseData()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = "", y = Budget, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Expense Distribution by Category", x = NULL, y = NULL) +
        theme_void() +
        theme(legend.title = element_blank())
    }
  })
  
  # Download CSV
  output$download <- downloadHandler(
    filename = function() { "expense_data.csv" },
    content = function(file) {
      write.csv(expenseData(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
