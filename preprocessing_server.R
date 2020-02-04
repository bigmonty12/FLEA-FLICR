# preprocessing_server.R
# Preprocessing tab of Shiny app

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

observeEvent(input$selectWellsButton, {
  updateTabItems(session, "tabs", "analysis")
})