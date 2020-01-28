library(shiny)
library(shinydashboard)
options(shiny.maxRequestSize=50*1024^2) 
names <- c("1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A", "5B", "6A", "6B")

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "FLIC Analysis"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Preprocess Data", tabName = "preprocessing", icon = icon("warehouse")),
      menuItem("Analysis", tabName = "analysis", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "preprocessing",
              fluidRow(
                box(fileInput("fin", "Select Raw FLIC file", accept = c('text/csv', '.csv')),
                    actionButton("submitButton", "Submit"),
                    width = 5)),
                # box(textInput("solutionA", "Solution A"),
                #     textInput("solutionB", "Solution B"),
                #     width = 4)),
              conditionalPanel(
                condition = "input.submitButton > 0",
                fluidRow(
                  box(plotOutput("rawPlots"), width = 12,
                      actionButton("subtractButton", "Subtract Baseline")))
              ),
              conditionalPanel(
                condition = "input.subtractButton > 0",
                fluidRow(
                  box(plotOutput("baselinePlots"), width = 12,
                      actionButton("removeBlipsButton", "Remove Blips & Analyze"))),
                fluidRow(
                  box(checkboxGroupInput("whichWells", "Wells to keep for analysis:",
                                         names, selected = names),
                      width = 2),
                  box(
                    textInput("solutionA", "Solution A"),
                    textInput("solutionB", "Solution B"),
                    width = 4
                  )
              ))),
      tabItem(tabName = "analysis",
              h2("Analysis"),
              fluidRow(
                box(textOutput("text"), width = 12)
              ))
    )
  )
)

server <- function(input, output, session){
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
  
  makeTimeDF <- reactive({
    fin <- readFile()
    # Create "Time" dataframe to use as x-axis in minutes
    time <- seq_len(length(fin[[1]])) / 300
    time_df <- data.frame(a=time, b=time, c=time, d=time, e=time, f=time, g=time, h=time, i=time, j=time, k=time, l=time)
  })
  
  getRawWells <- reactive({
    fin <- readFile()
    wells <- fin[5:16]
    names(wells) <- names
    wells
  })
  
  subtractBaseline <- reactive({
    wells <- getRawWells()
    # Use running median to find baseline for each well
    n <- length(wells[[1]])
    k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
    m <- min(k, 24001)
    baselines <- lapply(wells, runmed, k=m)
    # Subtract baseline from each well and change any negative values to 0
    subtractBaselines <- wells - baselines
  })
  
  output$rawPlots <- renderPlot({
    goOnFile()
    wells <- getRawWells()
    time_df <- makeTimeDF()
    par(mfcol = c(2, 6))
    Map(function(x,y,z) plot(x, y=y, main=z, col="red", type="l", xlab="Time [min]", ylab="Intensity [au]"),
        time_df, wells, names(wells))
  })
  
  goOnSubtract <- eventReactive(input$subtractButton, {
    input$fin
  })
  
  output$baselinePlots <- renderPlot({
    goOnSubtract()
    subtractBaselines <- subtractBaseline()
    time_df <- makeTimeDF()
    
    par(mfcol = c(2, 6))
    Map(function(x,y,z) plot(x, y=y, main=z, col="blue", type="l", xlab="Time [min]", ylab="Intensity [au]"),
        time_df, subtractBaselines, names(subtractBaselines))
  })
  
  observeEvent(input$removeBlipsButton, {
    updateTabItems(session, "tabs", "analysis")
  })
  
  goOnBlips <- eventReactive(input$removeBlipsButton, {
    input$fin
  })
  
  output$text <- renderText({
    goOnBlips()
    updateTabItems(session, "tabs", "analysis")
    whichWells <- paste(input$whichWells, collapse=", ")
    paste("Wells to be analyzed:", whichWells)
  })
}
shinyApp(ui = ui, server = server)