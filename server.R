# Load necessary libraries
library(shiny)
library(ggplot2)
library(DT)
library(leaflet)
library(httr)
library(jsonlite)
library(gemini.R)
library(tidygeocoder)
library(plotly)

# Function to get country name from city name
get_country_from_city <- function(city_name) {
  api_url <- paste0("http://api.geonames.org/searchJSON?q=", 
                    URLencode(city_name), "&maxRows=1&username=harshalk")
  response <- GET(api_url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    if (length(data$geonames) > 0) {
      return(data$geonames$countryName)
    } else {
      return("City not found.")
    }
  } else {
    return("API request failed.")
  }
}

# Function to get currency code based on country name
get_currency_code <- function(country_name) {
  api_url <- paste0("https://restcountries.com/v3.1/name/", URLencode(country_name))
  response <- GET(api_url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    currency_code <- names(data$currencies)[1]
    return(currency_code)
  } else {
    return("Invalid country or API request failed.")
  }
}

# Function to get exchange rate for the currency code
get_exchange_rate <- function(base_currency, target_currency) {
  api_url <- paste0("https://v6.exchangerate-api.com/v6/740bc68dfd27b836ad0c92b3/latest/", base_currency)
  response <- GET(api_url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    if (target_currency %in% names(data$conversion_rates)) {
      return(data$conversion_rates[[target_currency]])
    } else {
      return("Exchange rate not available for this currency.")
    }
  } else {
    return("Exchange rate API request failed.")
  }
}

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

# Function to get coordinates
get_coordinates_from_city <- function(city_name) {
  city_name <- data.frame(name = city_name)
  location <- geocode(city_name, address = name, method = "osm")
  lat <- location$lat
  long <- location$long
  
  return(c(lat, long))
}

# Function to get temperature forecast from coords
get_temp_from_coords <- function(lat, long, start_date, end_date) {
  # Fetch weather data
  url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=", 
                lat, "&longitude=", long, 
                "&daily=apparent_temperature_max,apparent_temperature_min&timezone=auto&start_date=", start_date, "&end_date=", end_date)
  response <- GET(url)
  
  # Check if the response is successful
  if (status_code(response) == 200) {
    # Parse JSON data
    data <- fromJSON(content(response, "text"))
    return(data)
  } else {
    return(NULL)
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
  
  # Define reactive values for the converted amount and display text
  converted_amount <- reactiveVal(NULL)
  converted_amount_display <- reactiveVal(NULL)
  
  # Handle "Go to Results" button
  observeEvent(input$goToResults, {
    # Get country names for both cities
    country1 <- get_country_from_city(input$location)
    country2 <- get_country_from_city(input$destination)
    
    # Get currency codes for both countries
    currency1 <- get_currency_code(country1)
    currency2 <- get_currency_code(country2)
    
    # Get exchange rate between the two currencies
    exchange_rate <- get_exchange_rate(currency1, currency2)
    
    if (currency1 != "Invalid country or API request failed." && 
        currency2 != "Invalid country or API request failed." && 
        exchange_rate != "Exchange rate API request failed.") {
      
      # Display converted amount
      converted_amount_value <- input$native_budget * as.numeric(exchange_rate)
      
      # Update the reactive values
      converted_amount(converted_amount_value)
      converted_amount_display(paste(formatC(converted_amount_value, format = "f", digits = 0), currency2))
      
      # Update the UI with exchange details
      output$native_amount_display <- renderText({
        paste(input$native_budget, currency1)
      })
      
      output$converted_amount_display <- renderText({
        converted_amount_display()
      })
      
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Unable to fetch currency or exchange rate data. Please check the entered locations.",
          easyClose = TRUE
        )
      )
    }
    
    latitude <- get_coordinates_from_city(input$destination)[1]
    longitude <- get_coordinates_from_city(input$destination)[2]
    
    start_date <- input$arrival_date
    end_date <- input$arrival_date + 6
    temp_json <- get_temp_from_coords(latitude, longitude, start_date, end_date)
    
    if (!is.null(temp_json)) {
      # Prepare the data frame
      df <- data.frame(
        Date = as.Date(temp_json$daily$time),
        Temperature_Max = temp_json$daily$apparent_temperature_max,
        Temperature_Min = temp_json$daily$apparent_temperature_min
      )
      
      # Create a unified hover text column
      df$hover_text <- paste0(
        "Date: ", df$Date, "<br>",
        "Max Temperature: ", df$Temperature_Max, "°C<br>",
        "Min Temperature: ", df$Temperature_Min, "°C"
      )
      
      # Plot using plotly
      output$temp_forecast<- renderPlotly({plot_ly(data = df, x = ~Date, y = ~Temperature_Max, type = "scatter", mode = "lines+markers",
                                                   name = "Max Temperature", line = list(color = "blue"), hoverinfo = "none") %>%
          add_trace(y = ~Temperature_Min, name = "Min Temperature", mode = "lines+markers",
                    line = list(color = "purple"), hoverinfo = "none") %>%
          add_trace(x = ~Date, y = ~Temperature_Max, type = "scatter", mode = "lines",
                    name = "", text = ~hover_text, hoverinfo = "text", showlegend = FALSE,
                    line = list(color = "rgba(0,0,0,0)")) %>%  # Invisible trace for hover
          layout(
            title = list(
              text = "Weather Forecast",
              font = list(
                color = "black",    # Font color
                weight = 'bold'     # Make the title bold
              )
            ),
            xaxis = list(title = "Date"),
            plot_bgcolor = 'rgba(0, 0, 0, 0)',  # Transparent background for the plot area
            paper_bgcolor = 'rgba(0, 0, 0, 0)',
            yaxis = list(title = "Temperature (°C)",
                         zeroline = FALSE),
            hovermode = "x unified"
          )
      })
      
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Please Enter the Arrival date less than 9 days from today due to API Limits.",
          easyClose = TRUE
        )
      )
    }
    
  })
  
  # Reactive value to store expenses
  expenses <- reactiveVal(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
  
  # Reactive value to store weather data
  weather_data <- reactiveVal(NULL)
  
  # Reactive value to store hotels data
  hotels <- reactiveVal(data.frame()) 
  
  # Classification logic for items
  setAPI("AIzaSyAwvmQJ9TzYWPrCV1gGbYG1k0YtayIEfIQ")
  
  # Classification logic using gemini
  classify_item <- function(item) {
    # Use gemini to classify the item
    category_save <- gemini(paste("There are some categories: Food, Luxury, Travel, Accommodation. Categorize", item, "into one category while travelling"))
    # Clean the output by removing any newline characters
    category_save <- gsub("\n", "", category_save)
    return(category_save)
  }
  
  # Shiny observeEvent for submitting new expenses
  observeEvent(input$submit_expense, {
    req(input$item, input$budget)
    
    if (input$budget <= 0) {
      showNotification("Please provide a valid budget.", type = "error")
      return()
    }
    
    # Categorize the item using gemini
    category <- classify_item(input$item)
    
    # Create a new expense data frame
    new_expense <- data.frame(
      Item = input$item,
      Category = category,
      Amount = input$budget,
      stringsAsFactors = FALSE
    )
    
    # Update the expenses data frame
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
    req(converted_amount())  # Ensure converted_amount is available
    converted_amount() - total_expenses()  # Use the converted amount instead of native_budget
  })
  
  
  # Reactive for biggest spending category
  # Reactive function to determine the biggest spending category
  biggest_spending_category <- reactive({
    expense_data <- expenses()  # Get the current expenses data
    if (nrow(expense_data) > 0) {
      # Calculate the total spending per category
      category_totals <- aggregate(Amount ~ Category, data = expense_data, sum)
      # Find the category with the maximum total amount
      category_totals$Category[which.max(category_totals$Amount)]
    } else {
      "None"  # Return "None" if there are no expenses
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
    req(input$native_budget)
    paste(formatC(converted_amount(), format = "f", digits = 0, big.mark = ","))
  })
  
  
  # Display Total Expenses
  output$total_expenses_display <- renderText({
    total_exp <- total_expenses()
    paste(formatC(total_exp, format = "f", digits = 0, big.mark = ","))
  })
  
  # Display Remaining Budget
  output$remaining_budget_display <- renderText({
    remaining <- remaining_budget()
    if (remaining < 0) {
      "Over Budget!"
    } else {
      paste(formatC(remaining, format = "f", digits = 0, big.mark = ","))
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
  output$overview_recommendations <- renderUI({
    req(input$destination, input$arrival_date, input$departure_date)
    location <- input$destination
    arrival_str <- as.character(input$arrival_date)
    departure_str <- as.character(input$departure_date)
    
    # Create the prompt for gemini
    prompt <- paste("I will be travelling from", arrival_str, "to", departure_str, 
                    "suggest me best places to visit in", location, 
                    "and the weather during these days. Strictly keep the output to: (place name) - (its historic/artistic importance), (why you should visit it), that's it nothing else.")
    
    tryCatch({
      # Fetch recommendations from gemini
      recommendations <- gemini(prompt)
      
      # Split recommendations into lines
      recommendations_list <- unlist(strsplit(recommendations, "\\)\\s*"))
      
      # Limit to two recommendations
      recommendations_list <- recommendations_list[1:2]
      
      # Generate HTML output for the first two recommendations
      html_output <- lapply(recommendations_list, function(item) {
        if (nchar(trimws(item)) > 0) {
          # Split into place name and description
          split_item <- unlist(strsplit(item, "-"))
          if (length(split_item) == 2) {
            tags$div(
              tags$b(trimws(split_item[1])),  # Bold place name
              tags$br(),
              tags$i(trimws(split_item[2])),  # Italic description
              tags$br(), tags$br()  # Add extra spacing
            )
          }
        }
      })
      
      # Add "More Recommendations" link
      html_output <- append(
        html_output,
        list(tags$div(
          tags$a("More Recommendations", href = "#", onclick = "Shiny.setInputValue('redirect_to_recommendations', true)")
        ))
      )
      
      # Return as HTML
      do.call(tagList, html_output)
      
    }, error = function(e) {
      tags$div("Unable to fetch recommendations at this time. Please try again later.")
    })
  })
  
  
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
  observeEvent(input$redirect_to_recommendations, {
    updateTabItems(session, "sidebarMenu", selected = "recommendations")
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
  # Reactive to monitor remaining budget
  observe({
    remaining <- remaining_budget()  # Replace with your logic to calculate remaining budget
    if (remaining < 0) {
      # Show modal dialog when out of budget
      showModal(modalDialog(
        title = "Out of Budget!",
        "Do you want us to recommend changes in your budget?",
        footer = tagList(
          modalButton("No"),  # Button to dismiss the dialog
          actionButton("yes_recommendations", "Yes", class = "btn-primary")  # Button to trigger recommendations
        )
      ))
    }
  })
  
  # Handle the "Yes" button click to show recommendations
  observeEvent(input$yes_recommendations, {
    removeModal()  # Close the first modal
    
    # Show recommendations modal
    showModal(modalDialog(
      title = "Budget Recommendations",
      tagList(
        # Recommendations UI
        h4("Suggested Adjustments:"),
        tabsetPanel(
          tabPanel("Spending Breakdown", plotOutput("spending_bar_chart")),
          tabPanel("Cheaper Accommodation", leafletOutput("cheap_accommodation_map", height = "400px")),
          tabPanel("Cheaper Places to Visit", leafletOutput("cheap_destinations_map", height = "400px")),
          tabPanel("Cheaper Transport", dataTableOutput("cheap_transport_table"))
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")  # Close button for recommendations modal
    ))
  })
  
  # Render spending breakdown chart 
  output$spending_bar_chart <- renderPlot({
    # Fetch the current expenses data
    spending_data <- expenses() 
    
    # Check if there are expenses recorded
    if (nrow(spending_data) > 0) {
      # Calculate the total spending per category
      category_totals <- aggregate(Amount ~ Category, data = spending_data, sum)
      
      # Identify the largest spending category
      largest_category <- category_totals$Category[which.max(category_totals$Amount)]
      
      # Create the bar chart
      ggplot(category_totals, aes(x = Category, y = Amount, fill = Category == largest_category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("grey", "blue"), labels = c("Other", "Largest")) +
        labs(title = "Spending by Category", x = "Category", y = "Amount") +
        theme_minimal() +
        theme(legend.position = "none")  # Remove the legend if not needed
    } else {
      # Placeholder plot when no data is available
      ggplot() +
        labs(title = "No Spending Data Available", x = "Category", y = "Amount") +
        theme_minimal()
    }
  })
  
  
  # Render cheaper accommodation map
  
  
  # Render cheaper accommodation map dynamically
  output$cheap_accommodation_map <- renderLeaflet({
    req(input$destination)
    
    # Define the prompt for Gemini
    accommodation_prompt <- paste(
      "Provide a list of affordable accommodations in",
      input$destination,
      "including their name, latitude, longitude, and price. Format the response as follows:",
      "(Hotel Name) - (latitude), (longitude), (price in local currency)."
    )
    
    tryCatch({
      # Fetch data from Gemini
      response <- gemini(accommodation_prompt)
      
      # Parse the response into a data frame
      accommodations <- do.call(rbind, lapply(strsplit(response, "\n"), function(line) {
        parts <- unlist(strsplit(line, "-|,"))
        data.frame(
          Name = trimws(parts[1]),
          Latitude = as.numeric(trimws(parts[2])),
          Longitude = as.numeric(trimws(parts[3])),
          Price = as.numeric(trimws(parts[4]))
        )
      }))
      
      # Plot accommodations on Leaflet map
      leaflet(accommodations) %>%
        addTiles() %>%
        addCircleMarkers(
          ~Longitude, ~Latitude,
          label = ~paste(Name, "Price:", Price),
          popup = ~paste("<b>", Name, "</b><br>Price: ", Price),
          color = "blue", radius = 5, fillOpacity = 0.7
        )
    }, error = function(e) {
      # Fallback: Display a blank map with an error message
      leaflet() %>% addTiles() %>% addPopups(0, 0, "Error fetching data. Please try again.")
    })
  })
  
  
  # Render cheaper transport options dynamically
  output$cheap_transport_table <- renderDataTable({
    req(input$destination)
    
    # Define the prompt for Gemini
    transport_prompt <- paste(
      "Provide a list of atleast three cheap transport options in",
      input$destination,
      "including their mode, price (in local currency), and availability. Format the response as follows:",
      "(Transport Mode) - (Price), (Availability)."
    )
    
    tryCatch({
      # Fetch data from Gemini
      response <- gemini(transport_prompt)
      
      # Parse the response into a data frame
      transport <- do.call(rbind, lapply(strsplit(response, "\n"), function(line) {
        parts <- unlist(strsplit(line, "-|,"))
        data.frame(
          Mode = trimws(parts[1]),
          Price = as.numeric(trimws(parts[2])),
          Availability = trimws(parts[3])
        )
      }))
      
      # Render transport options as a table
      datatable(
        transport,
        colnames = c("Transport Mode", "Cost (Local Currency)", "Availability"),
        options = list(pageLength = 5, autoWidth = TRUE)
      )
    }, error = function(e) {
      # Fallback: Return an empty table with an error message
      datatable(
        data.frame(
          Mode = "Error fetching data",
          Price = NA,
          Availability = NA
        ),
        colnames = c("Transport Mode", "Cost (Local Currency)", "Availability"),
        options = list(pageLength = 5, autoWidth = TRUE)
      )
    })
  })
}
