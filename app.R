library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(htmltools)
library(leaflet)
library(ggplot2)
library(owmr)
library(DT)

# Source external files
source("ui.R")
source("server.R")
source("expense_tracker.R")

# Initialize Shiny app
shinyApp(ui, server)
