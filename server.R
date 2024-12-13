library(ggplot2)
library(shinydashboard)
library(DT)
library(leaflet)
library(httr)
library(jsonlite)
library(gemini.R)
library(tidygeocoder)
library(plotly)
library(shinydashboardPlus)
library(shiny)
library(htmltools)

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
  # Open meteo API
  url <- paste0("https://api.open-meteo.com/v1/forecast?latitude=", 
                lat, "&longitude=", long, 
                "&daily=apparent_temperature_max,apparent_temperature_min&timezone=auto&start_date=", start_date, "&end_date=", end_date)
  response <- GET(url)
  
  if (status_code(response) == 200) {
    # Parse JSON data
    data <- fromJSON(content(response, "text"))
    return(data)
  } else {
    return(NULL)
  }
}

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
  ex_rate_key <- Sys.getenv("EX_RATE_KEY")
  api_url <- paste0("https://v6.exchangerate-api.com/v6/", ex_rate_key, "/latest/", base_currency)
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

# Function to fetch hotels from the Amadeus API
fetch_hotels <- function(lati,longi) {
  # Replace with your Amadeus API credentials
  client_id <- Sys.getenv("AMADEUS_ID_KEY")
  client_secret <- Sys.getenv("AMADEUS_SECRET_KEY")
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
  search_url <- paste0("https://test.api.amadeus.com/v1/reference-data/locations/hotels/by-geocode?latitude=", lati, "&longitude=", longi, "&radius=15&radiusUnit=KM&hotelSource=ALL")
  hotel_response <- GET(
    search_url,
    add_headers(Authorization = paste("Bearer", token))
  )
  
  if (status_code(hotel_response) == 200) {
    hotel_data <- fromJSON(content(hotel_response, as = "text"), flatten = TRUE)
    if (!is.null(hotel_data$data)) {
      # Extract relevant fields
      data.frame(
        name = hotel_data$data$name,
        lat = hotel_data$data$geoCode.latitude,
        lng = hotel_data$data$geoCode.longitude,
        country = hotel_data$data$address.countryCode,
        hotel_ids = hotel_data$data$hotelId,
        stringsAsFactors = FALSE
      )
    } else {
      stop("No hotel data found for the provided city code.")
    }
  } else {
    stop("Failed to fetch hotel data. Check the city code or API limits.")
  }
}
server <- function(input, output, session) {
  
  expenses <- reactiveVal(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
  
  reactive_values <- reactiveValues(currency2 = NULL)
  
  weather_data <- reactiveVal(NULL)
  
  hotels <- reactiveVal(data.frame()) 
  
  converted_amount <- reactiveVal(NULL)
  converted_amount_display <- reactiveVal(NULL)
  
  observeEvent(input$goToResults, {
    # Get country names for both cities
    country1 <- get_country_from_city(input$location)
    country2 <- get_country_from_city(input$destination)
    
    # Get currency codes for both countries
    currency1 <- get_currency_code(country1)
    currency2 <- get_currency_code(country2)
    
    reactive_values$currency2 <- currency2
    
    # Get exchange rate between the two currencies
    exchange_rate <- get_exchange_rate(currency1, currency2)
    
    if (currency1 != "Invalid country or API request failed." && 
        currency2 != "Invalid country or API request failed." && 
        exchange_rate != "Exchange rate API request failed.") {
      
      converted_amount_value <- input$native_budget * as.numeric(exchange_rate)
      
      converted_amount(converted_amount_value)
      converted_amount_display(paste(formatC(converted_amount_value, format = "f", digits = 0), currency2))
      
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
      df <- data.frame(
        Date = as.Date(temp_json$daily$time),
        Temperature_Max = temp_json$daily$apparent_temperature_max,
        Temperature_Min = temp_json$daily$apparent_temperature_min
      )
      
      df$hover_text <- paste0(
        "Date: ", df$Date, "<br>",
        "Max Temperature: ", df$Temperature_Max, "°C<br>",
        "Min Temperature: ", df$Temperature_Min, "°C"
      )
      
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
                color = "black",    
                weight = 'bold'    
              )
            ),
            xaxis = list(title = "Date"),
            plot_bgcolor = 'rgba(0, 0, 0, 0)',  
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
  
  
  expenses <- reactiveVal(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
  
  weather_data <- reactiveVal(NULL)
  
  hotels <- reactiveVal(data.frame()) 
  
  gemini_key <- Sys.getenv("GEMINI_KEY")
  setAPI(gemini_key)
  
  classify_item <- function(item) {
    category_save <- gemini(paste("There are some categories: Food, Luxury, Travel, Accommodation. Categorize", item, "into one category while travelling. Strictly only send the category nothing else."))
    category_save <- gsub("\n", "", category_save)
    return(category_save)
  }
  
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
  observeEvent(input$clear_expenses, {
    expenses(data.frame(Item = character(), Category = character(), Amount = numeric(), stringsAsFactors = FALSE))
    showNotification("All expenses cleared!", type = "message")
  })
  
  total_expenses <- reactive({
    sum(expenses()$Amount, na.rm = TRUE)
  })
  
  remaining_budget <- reactive({
    req(converted_amount())  
    converted_amount() - total_expenses()  
  })
  
  
  biggest_spending_category <- reactive({
    expense_data <- expenses()  
    if (nrow(expense_data) > 0) {
      category_totals <- aggregate(Amount ~ Category, data = expense_data, sum)
      category_totals$Category[which.max(category_totals$Amount)]
    } else {
      "None"  
    }
  })
  
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
    # OpenWeatherMap API
    weather_data_from_api <- tryCatch({
      owm_api_key <- Sys.getenv("OWM_API_KEY")
      owmr::owmr_settings(owm_api_key) 
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
    paste(formatC(converted_amount(), format = "f", digits = 0, big.mark = ","), reactive_values$currency2)
  })
  
  
  # Display Total Expenses
  output$total_expenses_display <- renderText({
    total_exp <- total_expenses()
    paste(formatC(total_exp, format = "f", digits = 0, big.mark = ","), reactive_values$currency2)
  })
  
  # Display Remaining Budget
  output$remaining_budget_display <- renderText({
    remaining <- remaining_budget()
    if (remaining < 0) {
      "Over Budget!"
    } else {
      paste(formatC(remaining, format = "f", digits = 0, big.mark = ","), reactive_values$currency2)
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
  
  output$expense_table <- renderTable({
    expenses()
  })
  
  # Expense Tracker Pie Chart
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
    
    prompt <- paste("I will be travelling from", arrival_str, "to", departure_str, 
                    "suggest me best places to visit in", location, 
                    "and the weather during these days. Strictly keep the output to: (place name) - (its historic/artistic importance), (why you should visit it), that's it nothing else.")
    
    tryCatch({
      recommendations <- gemini(prompt)
      
      recommendations_list <- unlist(strsplit(recommendations, "\\)\\s*"))
      
      recommendations_list <- recommendations_list[1:2]
      
      html_output <- lapply(recommendations_list, function(item) {
        if (nchar(trimws(item)) > 0) {
          split_item <- unlist(strsplit(item, "-"))
          if (length(split_item) == 2) {
            tags$div(
              tags$b(trimws(split_item[1])),  
              tags$br(),
              tags$i(trimws(split_item[2])),  
              tags$br(), tags$br()  
            )
          }
        }
      })
      
      html_output <- append(
        html_output,
        list(tags$div(
          tags$a("More Recommendations", href = "#", onclick = "Shiny.setInputValue('redirect_to_recommendations', true)")
        ))
      )
      
      do.call(tagList, html_output)
      
    }, error = function(e) {
      tags$div("Unable to fetch recommendations at this time. Please try again later.")
    })
  })
  
  
  hotels <- reactiveVal(data.frame())
  
  # Fetch hotel data
  observeEvent(input$update_hotels, {
    req(input$destination) 
    lati <- get_coordinates_from_city(input$destination)[1]
    longi <- get_coordinates_from_city(input$destination)[2]
    
    if (is.null(lati)) {
      showNotification("Invalid destination. Please check your city name.", type = "error")
      return()
    }
    
    tryCatch({
      hotel_data <- fetch_hotels(lati, longi)
      hotels(hotel_data)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  observeEvent(input$redirect_to_recommendations, {
    updateTabItems(session, "sidebarMenu", selected = "recommendations")
  })
  output$hotel_map <- renderLeaflet({
    req(nrow(hotels()) > 0) 
    leaflet(data = hotels()) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, popup = ~paste(name, "<br>Country:", country))
  })
  
  output$hotel_table <- DT::renderDataTable({
    req(nrow(hotels()) > 0) 
    hotels()
  })
  observe({
    remaining <- remaining_budget() 
    if (remaining < 0) {
      showModal(modalDialog(
        title = "Out of Budget!",
        "Do you want us to recommend changes in your budget?",
        footer = tagList(
          modalButton("No"), 
          actionButton("yes_recommendations", "Yes", class = "btn-primary") 
        )
      ))
    }
  })
  
  observeEvent(input$yes_recommendations, {
    removeModal()  
    
    showModal(modalDialog(
      title = "Budget Recommendations",
      tagList(
        h4("Suggested Adjustments:"),
        tabsetPanel(
          tabPanel("Spending Breakdown", plotOutput("spending_bar_chart")),
          tabPanel("Places to Visit", leafletOutput("cheap_destinations_map", height = "400px")),
          tabPanel("Cheaper Transport", dataTableOutput("cheap_transport_table"))
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")  
    ))
  })

  # Spending breakdown chart 
  output$spending_bar_chart <- renderPlot({
    spending_data <- expenses() 
    
    if (nrow(spending_data) > 0) {
      category_totals <- aggregate(Amount ~ Category, data = spending_data, sum)
      
      largest_category <- category_totals$Category[which.max(category_totals$Amount)]
      
      ggplot(category_totals, aes(x = Category, y = Amount, fill = Category == largest_category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("grey", "blue"), labels = c("Other", "Largest")) +
        labs(title = "Spending by Category", x = "Category", y = "Amount") +
        theme_minimal() +
        theme(legend.position = "none")  
    } else {
      ggplot() +
        labs(title = "No Spending Data Available", x = "Category", y = "Amount") +
        theme_minimal()
    }
  })
  
  
  
  
  output$cheap_destinations_map <- renderUI({
    req(input$destination, input$arrival_date, input$departure_date)
    
    location <- input$destination
    arrival_str <- as.character(input$arrival_date)
    departure_str <- as.character(input$departure_date)
    
    prompt <- paste("I will be travelling from", arrival_str, "to", departure_str, 
                    "suggest me best places to visit in", location, 
                    "and the weather during these days. Strictly keep the output to: (place name) - (its historic/artistic importance), (why you should visit it), that's it nothing else.")
    
    tryCatch({
      recommendations <- gemini(prompt)
      
      recommendations <- gsub("\\n+", " ", recommendations)
      
      recommendations_list <- unlist(strsplit(recommendations, "\\)\\s*"))
      
      recommendations_list <- recommendations_list[nchar(trimws(recommendations_list)) > 0]
      
      html_output <- lapply(recommendations_list, function(item) {

        
        split_item <- unlist(strsplit(item, "-", fixed = TRUE))
        if (length(split_item) >= 2) { 
          place_name <- trimws(split_item[1])
          description <- paste(trimws(split_item[-1]), collapse = "-")
          tags$div(
            tags$b(place_name),  
            tags$br(),
            tags$i(description),  
            tags$br(), tags$br()  
          )
        } else {
          
          NULL
        }
      })
      
      html_output <- Filter(Negate(is.null), html_output)
      
      do.call(tagList, html_output)
      
    }, error = function(e) {
      tags$div("Unable to fetch recommendations at this time. Please try again later.")
    })
  })
  
  
    
    
    
  output$cheap_transport_table <- renderDataTable({
    req(input$destination)
    
    transport_prompt <- paste(
      "Provide a list of atleast three cheap transport options in",
      input$destination,
      "including their mode, price (in local currency), and availability. Format the response as follows:",
      "(Transport Mode) - (Price). Strictly only provide the names and price and nothing else please, no introduction no conclusion nothing"
    )
    
    tryCatch({
      response <- gemini(transport_prompt)
      
      transport <- do.call(rbind, lapply(strsplit(response, "\n")[[1]], function(line) {
        parts <- unlist(strsplit(line, "-"))
        data.frame(
          Mode = trimws(parts[1]),
          Price = trimws(parts[2]),
          stringsAsFactors = FALSE
        )
      }))
      
      datatable(
        transport,
        colnames = c("Transport Mode", "Cost (Local Currency)"),
        options = list(pageLength = 5, autoWidth = TRUE)
      )
    }, error = function(e) {
      datatable(
        data.frame(
          Mode = "Error fetching data",
          Price = NA,
          stringsAsFactors = FALSE
        ),
        colnames = c("Transport Mode", "Cost (Local Currency)"),
        options = list(pageLength = 5, autoWidth = TRUE)
      )
    })
  })

  observeEvent(input$generate_itinerary, {
    req(input$destination, input$arrival_date, input$departure_date)
    
    remaining <- remaining_budget()
    if (is.null(remaining) || remaining <= 0) {
      output$formatted_itinerary <- renderUI({
        tags$div("Your remaining budget is insufficient to generate an itinerary. Please adjust your expenses.")
      })
      return()
    }
    
    if (is.null(reactive_values$currency2)) {
      output$formatted_itinerary <- renderUI({
        tags$div("Currency data is missing. Please try again later.")
      })
      return()
    }
    
    prompt <- paste(
      "Plan a trip itinerary for the following details:",
      sprintf("Destination: %s", input$destination),
      sprintf("Budget: %.2f %s", remaining, reactive_values$currency2),
      sprintf("Arrival Date: %s", input$arrival_date),
      sprintf("Departure Date: %s", input$departure_date),
      "Include daily activities and a cost breakdown for each day. Ensure the plan fits within the budget.",
      sep = "\n"
    )
    
    response <- tryCatch(
      gemini(prompt),
      error = function(e) {
        message("Error calling Gemini API: ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(response)) {
      output$formatted_itinerary <- renderUI({
        tags$div("Unable to generate itinerary at this time. Please try again later.")
      })
      return()
    }
    
    # Use regular expressions to apply consistent formatting to the text
    response <- gsub("\\*\\*([^\\*]+)\\*\\*", "<b>\\1</b>", response, perl = TRUE) 
    response <- gsub("(?m)^\\* ([^\\*]+)", "<li style='margin-bottom: 10px;'>\\1</li>", response, perl = TRUE) 
    response <- gsub("(?m)^##\\s*(.+)$", "<h2 style='font-size: 18px; font-weight: bold;'>\\1</h2>", response, perl = TRUE)
    response <- gsub("\n", "<br>", response)  
    
    
    output$formatted_itinerary <- renderUI({
      htmltools::HTML(
        paste0(
          '<div style="font-family: Arial, sans-serif; font-size: 14px; line-height: 1.8; color: #333; padding: 20px; background-color: #fdfdfd; border: 1px solid #ccc; border-radius: 10px; box-shadow: 0px 2px 5px rgba(0,0,0,0.1);">',
          response,
          '</div>'
        )
      )
    })
  })
}
