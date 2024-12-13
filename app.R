  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(htmltools)
  library(leaflet)
  library(ggplot2)
  library(owmr)
  library(DT)
    
  source("ui.R")
  source("server.R")
  source(".Renviron")
  
  shinyApp(ui, server)