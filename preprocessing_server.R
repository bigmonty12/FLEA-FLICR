# preprocessing_server.R
# Preprocessing tab of Shiny app

#====Functions used on input file====
source("beads.R")
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

baselineSteps <- function(x){
  n <- length(x[[1]])
  k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
  m <- min(k, 24001)
  # Use running median to find baseline for each well
  baselines1 <- lapply(x, runmed, k=m)
  baselines2 <- lapply(x - baselines1, beads, 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4)
  baselines3 <- rlist::list.map(baselines2, as.list(.[1]))
  baselines4 = unlist(baselines3, recursive = F)
  baselines5 <- lapply(baselines4, function(x) as.data.frame(as.matrix(x)))
  baselines6 <- as.data.frame(baselines5)
  names(baselines6) <- names
  baselines <- x - baselines6
  baselines
}

findBaseline <- reactive({
  wells <- getRawWells()
  baselines <- baselineSteps(wells)
  baselines
})

subtractBaseline <- reactive({
  wells <- getRawWells()
  baselines <- findBaseline()
  subtractBaselines <- wells - baselines
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
  par(mfcol = c(2, 6))
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
output$rawPlots <- renderPlot({
  goOnFile()
  rawPlots <- plotRaw()
  rawPlots
})

output$baselinePlots <- renderPlot({
  goOnFind()
  baselinePlots <- plotBaseline()
  baselinePlots
})

output$subtractBaselinePlots <- renderPlot({
  goOnSubtract()
  subtractPlots <- plotSubtractBaseline()
  subtractPlots
})

output$removedBlipsPlots <- renderPlot({
  goOnBlips()
  removedBlips <- plotRemovedBlips()
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
    plotBaseline()
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