# preprocessing_server.R
# Preprocessing tab of Shiny app

#====Functions used on input file====
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
  # Convert all values < 10 to zero
  subtractBaselines[subtractBaselines < 10] <- 0
  subtractBaselines
})

plotwells <- function(a, b){
  data <- a()
  time_df <- makeTimeDF()
  par(mfcol = c(2, 6))
  Map(function(x, y, z) plot(x, y=y, main=z, col=b, type="l",
                             xlab="Time [min]", ylab="Intensity [au]"),
      time_df, data, names(data))
}

plotRaw <- function(){
  plots <- plotwells(getRawWells, "red")
  plots
}

plotBaseline <- function(){
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
  goOnSubtract()
  baselinePlots <- plotBaseline()
  baselinePlots
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

output$downloadSubtracted <- downloadHandler(
  filename = function() {
    file = paste0("baselinePlots_", fileName(), ".png")
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