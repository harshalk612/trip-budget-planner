# Expense Tracker UI
expense_tracker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(ns("item"), "Enter Item Name:", ""),
        numericInput(ns("amount"), "Enter Amount:", value = 0, min = 0),
        actionButton(ns("add"), "Add Expense"),
        actionButton(ns("clear"), "Clear All Expenses"),
        hr(),
        tableOutput(ns("expense_table"))
      ),
      mainPanel(
        plotOutput(ns("expense_plot")),
        downloadButton(ns("download"), "Download Expenses")
      )
    )
  )
}

# Expense Tracker Server
expense_tracker_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store expenses
    expenses <- reactiveVal(data.frame(Item = character(), Amount = numeric(), stringsAsFactors = FALSE))
    
    # Add new expense
    observeEvent(input$add, {
      if (input$item == "" || input$amount <= 0) {
        showNotification("Please provide a valid item and amount.", type = "error")
        return()
      }
      new_expense <- data.frame(Item = input$item, Amount = input$amount, stringsAsFactors = FALSE)
      expenses(rbind(expenses(), new_expense))
    })
    
    # Clear all expenses
    observeEvent(input$clear, {
      expenses(data.frame(Item = character(), Amount = numeric(), stringsAsFactors = FALSE))
    })
    
    # Render expense table
    output$expense_table <- renderTable({
      expenses()
    })
    
    # Render expense plot
    output$expense_plot <- renderPlot({
      expense_data <- expenses()
      if (nrow(expense_data) > 0) {
        barplot(
          height = expense_data$Amount,
          names.arg = expense_data$Item,
          col = "blue",
          main = "Expenses",
          xlab = "Items",
          ylab = "Amount"
        )
      }
    })
    
    # Download expenses as CSV
    output$download <- downloadHandler(
      filename = function() {
        paste("expenses-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(expenses(), file, row.names = FALSE)
      }
    )
  })
}

