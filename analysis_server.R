# analysis_server.R
# Analysis tab of Shiny app

#====Allow user input of pheno/genotypes====
# How many different pheno/genotypes
findNumTypes <- reactive({
  numTypes <- as.integer(input$numTypes)
})

# Create user assigned number of textInputs to put in pheno/genotypes
output$types <- renderUI({
  numTypes <- findNumTypes()
  lapply(1:numTypes, function(i) {
    textInput(inputId = paste0("type", i), 
              label = paste("Genotype/Phenotype #", i))
  })
})

# Create list of pheno/genotypes
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
#====Create 6 Wells to choose pheno/genotype====
lapply(1:6, function(i) {
  output[[paste0("well", i)]] <- renderUI({
    types <- numTypesList()
    selectInput(paste0("well", i), paste0("Well ", i), choices = types)
  })
})
#====Convert data to none, leg, or probosic event====
editEvents <- function(x) {
  runs <- rle(as.integer(x))
  
  for (i in seq_along(runs$values)) {
    # leg events sandwiching a proboscis event are converted to proboscis events
    if (runs$values[i] == 2) {
      if (runs$values[i-1] == 1) runs$values[i-1] = 2
      if (runs$values[i+1] == 1) runs$values[i+1] = 2
    }
  }
  new_runs <- rle(inverse.rle(runs))
  for (i in seq_along(new_runs$values)) {
    if (new_runs$values[i] == 1) {
      # if "leg event" is longer than 4 sec, change to none event
      if (new_runs$lengths[i] > 4*5) new_runs$values[i] = 0
    }
    if (new_runs$values[i] == 2) {
      # if "proboscis event" is longer than 40 sec, change to none event
      if (new_runs$lengths[i] > 40*5) new_runs$values[i] = 0
    }
  }
  return(inverse.rle(new_runs))
}

labelEvents <- reactive({
  labeledEvents <- removeBlips()
  labeledEvents[labeledEvents > 0 & labeledEvents < 100] <- 1
  labeledEvents[labeledEvents >= 100] <- 2
  labeledEvents
})

editedEvents <- reactive({
  labeledEvents <- labelEvents()
  editedEvents <- as.data.frame(lapply(labeledEvents, editEvents))
  editedEvents
})

goOnAnalyzeData <- eventReactive(input$analyzeDataButton, {
  input$fin
})

output$editedEvents <- renderTable({
  goOnAnalyzeData()
  editedEvents <- editedEvents()
  editedEvents
})