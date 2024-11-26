library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(htmltools)
library(ggplot2)
library(owmr)

source("expense_tracker.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
