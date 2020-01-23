library(shiny)
library(shinydashboard)
options(shiny.maxRequestSize=50*1024^2) 
wells <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12")


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "FLIC Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Preprocess Data", tabName = "preprocessing", icon = icon("warehouse")),
      menuItem("Analysis", tabName = "analysis", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "preprocessing",
              fluidRow(
                box(fileInput("fin", "Select Raw FLIC file", accept = c('text/csv', '.csv')),
                    actionButton("submitButton", "Submit"))),
              conditionalPanel(
                condition = "input.submitButton > 0",
                fluidRow(
                  box(plotOutput("rawPlots"), width = 10,
                      actionButton("subtractButton", "Subtract Baseline")))
              ),
              conditionalPanel(
                condition = "input.subtractButton > 0",
                fluidRow(
                  box(checkboxGroupInput("whichWells", "Wells to keep for analysis:",
                                         wells, selected = wells), width = 2),
                  box(plotOutput("baselinePlots"), width = 10)
                )
              )),
      tabItem(tabName = "analysis",
              h2("Analysis"))
    )
  )
)

server <- function(input, output){
  inFile <- reactive(input$fin)
  
  goOnFile <- eventReactive(input$submitButton, {
    input$fin
  })
  
  readFile <- reactive({
    rawFile <- input$fin
    
    if (is.null(rawFile))
      return(NULL)
    
    isolate({
      raw <- read.csv(rawFile$datapath)
      raw
    })
  })
  
  output$rawPlots <- renderPlot({
    goOnFile()
    fin <- readFile()
    wells <- fin[5:16]
    # plot(wells[[1]], col="red", type="l")
    # dev.off()
    par(mfcol = c(2, 6))
    Map(function(x,y) plot(x, main =y, col="red", type="l"), wells, names(wells))
  })
  
  goOnSubtract <- eventReactive(input$subtractButton, {
    input$fin
  })
  
  output$baselinePlots <- renderPlot({
    goOnSubtract()
    fin <- readFile()
    wells <- fin[5:16]
    # Use running median to find baseline for each well
    n <- length(wells$W1)
    k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
    m <- min(k, 24001)
    baselines <- lapply(wells, runmed, k=m)
    # Subtract baseline from each well and change any negative values to 0
    subtractBaselines <- wells - baselines
    #dev.off()
    par(mfcol = c(2, 6))
    Map(function(x,y) plot(x, main =y, col="blue", type="l"), subtractBaselines, names(subtractBaselines))
  })
}
shinyApp(ui = ui, server = server)