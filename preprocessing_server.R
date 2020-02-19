# preprocessing_server.R
# Preprocessing tab of Shiny app

#====Functions used on input file====
inFile <- reactive(input$fin)

goOnFile <- eventReactive(input$submitButton, {
  input$fin
})

BinMean <- function (vec, every, na.rm = FALSE) {
  n <- length(vec)
  x <- .colMeans(vec, every, n %/% every, na.rm)
  r <- n %% every
  if (r) x <- c(x, mean.default(vec[(n - r + 1):n], na.rm = na.rm))
  x
}

readFile <- reactive({
  rawFile <- input$fin
  
  if (is.null(rawFile))
    return(NULL)
  isolate({
    if (input$flicFlea == "FLEA") {
      raw1 <- read.csv(rawFile$datapath)
      raw <- as.data.frame(lapply(raw1[2:9], BinMean, every=100)) * 310
      rm(raw1)
    }
    else {
      raw <- read.csv(rawFile$datapath)
    } 
    raw
  })
})

fileName <- reactive({
  rawFile <- input$fin
  fileName <- stringi::stri_extract_first(str = rawFile$name, regex = ".*(?=\\.)")
  fileName
})
#====Button pressing functions====
goOnFind <- eventReactive(input$findButton, {
  input$fin
})

goOnSubtract <- eventReactive(input$subtractButton, {
  input$fin
})

goOnBlips <- eventReactive(input$removeBlipsButton, {
  input$fin
})

goOnSelectWells <- eventReactive(input$selectWellsButton, {
  input$fin
})

observeEvent(input$selectWellsButton, {
  updateTabItems(session, "tabs", "analysis")
})
#====Functions used for plotting====
whichNames <- reactive({
  if (input$flicFlea == "FLIC"){
    names <- namesFLIC
  } else {
    names <- namesFLEA
  }
  names
})

makeTimeDF <- reactive({
  fin <- readFile()
  # Create "Time" dataframe to use as x-axis in minutes
  time <- seq_len(length(fin[[1]])) / 300
  if (input$flicFlea == "FLIC") {
    time_df <- data.frame(a=time, b=time, c=time, d=time, e=time, f=time, g=time, h=time, i=time, j=time, k=time, l=time)
  } else {
    time_df <- data.frame(a=time, b=time, c=time, d=time, e=time, f=time, g=time, h=time)
  }
})

wellNums <- reactive({
  if (input$flicFlea == "FLIC"){
    wells <- 6
  } else {
    wells <- 4
  }
  wells
})



getRawWells <- reactive({
  fin <- readFile()
  if (input$flicFlea == "FLIC"){
    wells <- fin[5:16]
  }
  else {
    wells <- fin[1:8]
  }
  names(wells) <- whichNames()
  wells
})

baselineRunMed <- function(x){
  n <- length(x[[1]])
  k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
  m <- min(k, 24001)
  # Use running median to find baseline for each well
  baselines <- lapply(x, runmed, k=m)
  baselines
}

baselineBeads <- function(x){
  source("beads.R")
  baselines1 <- baselineRunMed(x)
  baselines2 <- lapply(x - baselines1, beads, 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4)
  baselines3 <- rlist::list.map(baselines2, as.list(.[1]))
  baselines4 = unlist(baselines3, recursive = F)
  baselines5 <- lapply(baselines4, function(x) as.data.frame(as.matrix(x)))
  baselines6 <- as.data.frame(baselines5)
  names(baselines6) <- whichNames()
  baselines <- x - baselines6
  rm(baselines1, baselines2, baselines3, baselines4, baselines5, baselines6)
  baselines
}

findBaseline <- reactive({
  if (input$baselineMethod == "Running Median (fast, less precise)"){
    baselines <- as.data.frame(baselineRunMed(getRawWells()))
  } else {
    baselines <- baselineBeads(getRawWells())
  }
  baselines
})

subtractBaseline <- reactive({
  subtractBaselines <- getRawWells() - findBaseline()
})

removeBlips <- reactive({
  subtractBaselines <- subtractBaseline()
  # Convert all values < 10 to zero
  subtractBaselines[subtractBaselines < 10] <- 0
  subtractBaselines
})

plotData <- function(x, y, z, col){
  plot(x, y=y, main=z, col=col, type="l",
       xlab="Time [min]", ylab="Intensity [au]")
}

plotDataAndLine <- function(x, y, z, col, baseline){
  plot(x, y=y, main=z, col=col, type="l",
       xlab="Time [min]", ylab="Intensity [au]")
  lines(x=x, y=baseline, lwd=2, col="blue")
}

plotwells <- function(a, col, plotLine=FALSE){
  data <- a()
  time_df <- makeTimeDF()
  par(mfcol = c(2, wellNums()))
  if (plotLine==FALSE){
    Map(plotData, time_df, data, names(data), col)
  } else {
    baseline <- findBaseline()
    Map(plotDataAndLine, time_df, data, names(data), col, baseline)
  }
}

plotRaw <- function(){
  plots <- plotwells(getRawWells, "red")
  plots
}

plotBaseline <- function(){
  plots <- plotwells(getRawWells, "red", plotLine = TRUE)
  plots
}

plotSubtractBaseline <- function(){
  plots <- plotwells(subtractBaseline, "blue")
  plots
}

plotRemovedBlips <- function(){
  plots <- plotwells(removeBlips, "forestgreen")
  plots
}
#====Render plots====
rawW <- waiter::Waiter$new(id="rawPlots", html = spin_pulsar())
output$rawPlots <- renderPlot({
  goOnFile()
  rawW$show()
  rawPlots <- plotRaw()
  print(pryr::mem_used())
  rawPlots
})

baselineW <- waiter::Waiter$new(id="baselinePlots", html = spin_pulsar())
output$baselinePlots <- renderPlot({
  goOnFind()
  baselineW$show()
  baselinePlots <- plotBaseline()
  print(pryr::mem_used())
  output$rawPlots <- NULL
  baselinePlots
})

subtractW <- waiter::Waiter$new(id="subtractBaselinePlots", html = spin_pulsar())
output$subtractBaselinePlots <- renderPlot({
  goOnSubtract()
  subtractW$show()
  subtractPlots <- plotSubtractBaseline()
  print(pryr::mem_used())
  output$baselinePlots <- NULL
  subtractPlots
})

removedW <- waiter::Waiter$new(id="removedBlipsPlots", html = spin_pulsar())
output$removedBlipsPlots <- renderPlot({
  goOnBlips()
  removedW$show()
  removedBlips <- plotRemovedBlips()
  print(pryr::mem_used())
  output$subtractBaselinePlots <- NULL
  removedBlips
})
#====Download plots====
output$downloadRawPlots <- downloadHandler(
  filename = function() {
    file = paste0("rawPlots_", fileName(), ".png")
  },
  content = function(file){
    png(file=file, height = 480*1.5, width = 480*3)
    plotRaw()
    dev.off()
  }
)

output$downloadBaseline <- downloadHandler(
  filename = function() {
    file = paste0("baselinePlots_", fileName(), ".png")
  },
  content = function(file){
    png(file=file, height = 480*1.5, width = 480*3)
    plotBaseline()
    dev.off()
  }
)

output$downloadSubtracted <- downloadHandler(
  filename = function() {
    file = paste0("subtractedPlots_", fileName(), ".png")
  },
  content = function(file){
    png(file=file, height = 480*1.5, width = 480*3)
    plotSubtractBaseline()
    dev.off()
  }
)

output$downloadRemovedBlips <- downloadHandler(
  filename = function() {
    file = paste0("removedBlipsPlots_", fileName(), ".png")
  },
  content = function(file){
    png(file=file, height = 480*1.5, width = 480*3)
    plotRemovedBlips()
    dev.off()
  }
)