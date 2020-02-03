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
                    actionButton("submitButton", "Submit and View Wells"),
                    width = 5)),
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
                      actionButton("removeBlipsButton", "Remove Blips")))),
              conditionalPanel(
                condition = "input.removeBlipsButton > 0",
                fluidRow(
                  box(plotOutput("removedBlipsPlots"), width = 12)
                ),
                fluidRow(
                  box(checkboxGroupInput("whichWells", "Wells to keep for analysis:",
                                         names, selected = names, inline = TRUE),
                      actionButton("selectWellsButton", "Select Wells and Go to Analysis"),
                      width = 12
                      )
                ))),
      tabItem(tabName = "analysis",
              h2("Analysis"),
              fluidRow(
                # box(
                #   textInput("solutionA", "Solution A"),
                #   textInput("solutionB", "Solution B"),
                #   width = 5
                # ),
                  box(
                    numericInput("numTypes", "Number of genotypes/phenotypes", value = 2),
                    uiOutput("types", inline = TRUE),
                    actionButton("describeWellsButton", "Assign Wells"),
                    width = 7)
              ),
              conditionalPanel(
                condition = "input.describeWellsButton > 0",
                fluidRow(
                  box(
                    uiOutput("well1"),
                    uiOutput("well2"),
                    uiOutput("well3"),
                    uiOutput("well4"),
                    uiOutput("well5"),
                    uiOutput("well6")
                  )
                )
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
  
  removeBlips <- reactive({
    subtractBaselines <- subtractBaseline()
    subtractBaselines[subtractBaselines < 10] <- 0
    subtractBaselines
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
  
  # observeEvent(input$removeBlipsButton, {
  #   updateTabItems(session, "tabs", "analysis")
  # })
  
  goOnBlips <- eventReactive(input$removeBlipsButton, {
    input$fin
  })
  
  goOnSelectWells <- eventReactive(input$selectWellsButton, {
    input$fin
  })
  
  output$removedBlipsPlots <- renderPlot({
    goOnBlips()
    removeBlips <- removeBlips()
    time_df <- makeTimeDF()
    
    par(mfcol = c(2, 6))
    Map(function(x,y,z) plot(x, y=y, main=z, col="forestgreen", type="l", xlab="Time [min]", ylab="Intensity [au]"),
        time_df, removeBlips, names(removeBlips))
  })
  
  # output$text <- renderText({
  #   goOnBlips()
  #   updateTabItems(session, "tabs", "analysis")
  #   whichWells <- paste(input$whichWells, collapse=", ")
  #   paste("Wells to be analyzed:", whichWells)
  # })
  
  observeEvent(input$selectWellsButton, {
    updateTabItems(session, "tabs", "analysis")
  })
  
  findNumTypes <- reactive({
    numTypes <- as.integer(input$numTypes)
  })
  
  output$types <- renderUI({
    numTypes <- findNumTypes()
    lapply(1:numTypes, function(i) {
      textInput(inputId = paste0("type", i), 
                label = paste("Genotype/Phenotype #", i))
    })
  })

  numTypesList <- reactive({
    numTypes <- findNumTypes()
    types <- list()
    lapply(1:numTypes, function(i) {
      types[paste0('type', i)] <- input[[paste0('type', i)]]
    })
  })
  
  eventReactive(input$describeWellsButton, {
    input$fin
  })
  
  output$well1 <- renderUI({
    types <- numTypesList()
    selectInput("well1", "Well 1", choices = types)
  })
  
  output$well2 <- renderUI({
    types <- numTypesList()
    selectInput("well2", "Well 2", choices = types)
  })
  
  output$well3 <- renderUI({
    types <- numTypesList()
    selectInput("well3", "Well 3", choices = types)
  })
  
  output$well4 <- renderUI({
    types <- numTypesList()
    selectInput("well4", "Well 4", choices = types)
  })
  
  output$well5 <- renderUI({
    types <- numTypesList()
    selectInput("well5", "Well 5", choices = types)
  })
  
  output$well6 <- renderUI({
    types <- numTypesList()
    selectInput("well6", "Well 6", choices = types)
  })
}
shinyApp(ui = ui, server = server)