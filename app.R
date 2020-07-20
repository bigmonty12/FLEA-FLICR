# FLIC app created by Austin Montgomery
# Feb 2020
print(pryr::mem_used())
library(shiny)
library(shinydashboard)
options(shiny.maxRequestSize=1000*1024^2)
library(dplyr)
library(DT)
library(waiter)
print(pryr::mem_used())
namesFLIC <- c("1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A", "5B", "6A", "6B")
namesFLEA <- c("1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B")
source("FLEA-FLICR-functions.R", local = TRUE)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "FLEA-FLICR"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Preprocess Data", tabName = "preprocessing", icon = icon("warehouse")),
      menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
      menuItem("Results", tabName = "results", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      source("preprocessing_ui.R", local = TRUE)$value,
      source("analysis_ui.R", local = TRUE)$value,
      source("results_ui.R", local = TRUE)$value
    )
  )
)

server <- function(input, output, session){

  source("preprocessing_server.R", local = TRUE)$value  
  source("analysis_server.R", local = TRUE)$value
  source("results_server.R", local = TRUE)$value
}

shinyApp(ui = ui, server = server)