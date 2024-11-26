# Load necessary libraries
library(shiny)
library(ggplot2)
library(DT)
library(leaflet)
library(httr)
library(jsonlite)

# Sample list of IATA codes and their city names
iata_codes <- data.frame(
  city = c("New York", "Los Angeles", "London", "Tokyo", "Surat", "Paris"),
  iata = c("JFK", "LAX", "LHR", "HND", "STV", "PAR"),
  stringsAsFactors = FALSE
)

# Function to get IATA code from city name
get_iata_code <- function(city) {
  match <- iata_codes[iata_codes$city == city, "iata"]
  if (length(match) > 0) {
    return(match)
  } else {
    return(NULL)
  }
}

# Function to fetch hotels from the Amadeus API
fetch_hotels <- function(iata_code) {
  # Replace with your Amadeus API credentials
  client_id <- "iqjW3ZwNuQOGhthz4AJQDoDGsLoecFkP"
  client_secret <- "vx8wTLLFLOGyRR8i"
  
  # Get access token
  token_url <- "https://test.api.amadeus.com/v1/security/oauth2/token"
  token_response <- POST(
    token_url,
    body = list(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token <- content(token_response)$access_token
  } else {
    stop("Failed to retrieve access token. Check your API credentials.")
  }
  
  # Fetch hotels by city code
  search_url <- paste0("https://test.api.amadeus.com/v1/reference-data/locations/hotels/by-city?cityCode=", toupper(iata_code))
  hotel_response <- GET(
    search_url,
    add_headers(Authorization = paste("Bearer", token))
  )
  
  # Parse response
  if (status_code(hotel_response) == 200) {
    hotel_data <- fromJSON(content(hotel_response, as = "text"), flatten = TRUE)
    if (!is.null(hotel_data$data)) {
      # Extract relevant fields
      data.frame(
        name = hotel_data$data$name,
        lat = hotel_data$data$geoCode.latitude,
        lng = hotel_data$data$geoCode.longitude,
        country = hotel_data$data$address.countryCode,
        stringsAsFactors = FALSE
      )
    } else {
      stop("No hotel data found for the provided city code.")
    }
  } else {
    stop("Failed to fetch hotel data. Check the city code or API limits.")
  }
}

# Define Server
server <- function(input, output, session) {
  # Reactive value to store expenses
  expenses <- reactiveVal(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
  
  # Reactive value to store weather data
  weather_data <- reactiveVal(NULL)
  
  # Reactive value to store hotels data
  hotels <- reactiveVal(data.frame()) 
  
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
    req(input$item, input$budget)
    if (input$budget <= 0) {
      showNotification("Please provide a valid budget.", type = "error")
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
    req(input$total_budget)
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
    # Fetch weather data from OpenWeatherMap API
    weather_data_from_api <- tryCatch({
      owmr::owmr_settings("109d734da4f858e9dca6534a23b73e2b") # Replace with your OpenWeatherMap API key
      owmr::get_current(input$destination, units = "metric")
    }, error = function(e) {
      NULL
    })
    weather_data(weather_data_from_api)
    updateTabItems(session, "sidebarMenu", selected = "results")
  })
  
  # Display Total Budget
  output$total_budget_display <- renderText({
    req(input$total_budget)
    paste("$", formatC(input$total_budget, format = "f", digits = 0, big.mark = ","))
  })
  
  # Display Total Expenses
  output$total_expenses_display <- renderText({
    total_exp <- total_expenses()
    paste("$", formatC(total_exp, format = "f", digits = 0, big.mark = ","))
  })
  
  # Display Remaining Budget
  output$remaining_budget_display <- renderText({
    remaining <- remaining_budget()
    if (remaining < 0) {
      "Over Budget!"
    } else {
      paste("$", formatC(remaining, format = "f", digits = 0, big.mark = ","))
    }
  })
  
  # Display Biggest Spending Category
  output$biggest_spending_display <- renderText({
    biggest_spending_category()
  })
  
  # Display Weather Forecast
  output$weather_display <- renderText({
    req(input$destination)
    weather <- weather_data()
    if (!is.null(weather)) {
      temp <- weather$main$temp
      paste("Temperature:", temp, "°C")
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
  
  hotels <- reactiveVal(data.frame())
  
  # Observe button click to fetch hotel data
  observeEvent(input$update_hotels, {
    req(input$destination) # Ensure the destination is entered
    iata_code <- get_iata_code(input$destination)
    
    if (is.null(iata_code)) {
      showNotification("Invalid destination. Please check your city name.", type = "error")
      return()
    }
    
    tryCatch({
      hotel_data <- fetch_hotels(iata_code)
      hotels(hotel_data)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Render Leaflet map
  output$hotel_map <- renderLeaflet({
    req(nrow(hotels()) > 0) # Ensure there is data to display
    leaflet(data = hotels()) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, popup = ~paste(name, "<br>Country:", country))
  })
  
  # Render data table
  output$hotel_table <- DT::renderDataTable({
    req(nrow(hotels()) > 0) # Ensure there is data to display
    hotels()
  })
}
