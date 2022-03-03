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

observe({
  shinyjs::toggleState("describeWellsButton", !is.null(input$type1) && input$type1 != "")
})

eventReactive(input$describeWellsButton, {
  input$fin
})
#====Create Wells to choose pheno/genotype====
observeEvent(input$describeWellsButton, {
  lapply(1:wellNums(), function(i) {
    output[[paste0("well", i)]] <- renderUI({
      types <- numTypesList()
      selectInput(paste0("well", i), paste0("Well ", i), choices = types)
    })
  })
})

labelEvents <- reactive({
  labeledEvents <- removeBlips()
  if (input$flicFlea == "FLIC"){
    labeledEvents[labeledEvents > 0 & labeledEvents < 100] <- 1
    labeledEvents[labeledEvents >= 100] <- 2
  } else {
    probCutoffA <- car::recode(input$resistanceA, "'3.3 MOhm'=40; '4.7 MOhm'=56; '10 MOhm'=100; '20 MOhm'=155; '33 MOhm'=190;")
    probCutoffB <- car::recode(input$resistanceB, "'3.3 MOhm'=40; '4.7 MOhm'=56; '10 MOhm'=100; '20 MOhm'=155; '33 MOhm'=190;")
    oddEvents <- labeledEvents[c(1,3,5,7)]
    evenEvents <- labeledEvents[c(2,4,6,8)]
    oddEvents[oddEvents > 0 & oddEvents < probCutoffA] <- 1
    oddEvents[oddEvents >= probCutoffA] <- 2
    evenEvents[evenEvents > 0 & evenEvents < probCutoffB] <- 1
    evenEvents[evenEvents >= probCutoffB] <- 2
    labeledEvents <- cbind(oddEvents, evenEvents) %>% select(
      "1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B"
    )
    rm(oddEvents, evenEvents)
  }
  labeledEvents
})

editedEvents <- reactive({
  editedEvents <- as.data.frame(lapply(labelEvents(), editEvents))
  editedEvents
})

# Create data frame for each event

wellEncodedTimeEvents <- reactive({
  # Add date and time back to events
  editedEventsTime <- cbind(readFile()[,1:3], editedEvents())
  # Column names of df
  names(editedEventsTime) <- c(c("Date", "Time", "MSec"), namesFLIC)
  # Remove rows where there is no activity in any wells
  keepOnlyEvents <- editedEventsTime %>% filter(rowSums(editedEventsTime[,4:15]) > 0)
  
  # Keep time when event begins
  idx <- unlist(lapply(keepOnlyEvents[,4:15], function(x){
    (cumsum(rle(x)[[1]]))[rle(x)[[2]]>0] - rle(x)[[1]][rle(x)[[2]]>0]
  })) + 1
  
  # Length of bout * 0.2 ms
  boutLength <- unlist(lapply(keepOnlyEvents[,4:15], function(x){
    rle(x)[[1]][rle(x)[[2]]>0] * 0.2
  }))
  
  # Leg vs proboscis event
  boutType <- unlist(lapply(keepOnlyEvents[,4:15], function(x){
    rle(x)[[2]][rle(x)[[2]]>0]
  }))
  
  # Specify well where event occurs
  boutWell <- rep(namesFLIC, unlist((lapply(keepOnlyEvents[,4:15], function(x){
    length(rle(x)[[2]][rle(x)[[2]]>0])
  }))))
  
  # Merge previous vectors and merge date/time into one column
  timeLengthTypeWell <- cbind(keepOnlyEvents[idx,1:3], boutLength, boutType, boutWell) %>%
    mutate(Time = strptime(paste(Date, Time), format="%m/%d/%Y %H:%M:%S") + MSec*10^-3,
      .keep = "unused",
      .before = boutLength
      ) %>%
    arrange(Time)
  
  # Keep milliseconds data
  options("digits.secs"=6)
  
  # Create matrix with each well having its own column
  wideEvents <- timeLengthTypeWell %>%
    spread(boutWell, boutLength, fill=NA)
  
  wideEvents
})

analyzedEvents <- reactive({
  analyzedEvents <- lapply(removeUntilFirstEvent(editedEvents(), input$length, wellNums(), whichNames()), analyzeEvents)
  analyzedEvents <- as.data.frame(do.call(rbind, analyzedEvents))
  condition <- c(paste0(input$well1, ": ", input$solutionA), 
                 paste0(input$well1, ": ", input$solutionB),
                 paste0(input$well2, ": ", input$solutionA), 
                 paste0(input$well2, ": ", input$solutionB),
                 paste0(input$well3, ": ", input$solutionA), 
                 paste0(input$well3, ": ", input$solutionB),
                 paste0(input$well4, ": ", input$solutionA), 
                 paste0(input$well4, ": ", input$solutionB)
  )
  if (input$flicFlea == "FLIC") {
    condition <- append(condition, c(paste0(input$well5, ": ", input$solutionA), 
                                     paste0(input$well5, ": ", input$solutionB),
                                     paste0(input$well6, ": ", input$solutionA), 
                                     paste0(input$well6, ": ", input$solutionB)
                                     )
                        )
  }
  
  analyzedEvents['Condition'] <- condition
  analyzedEvents['Minutes.Analyzed'] <- rep(timeAnalyzed(removeUntilFirstEvent(editedEvents(), input$length, wellNums(), whichNames())), 
                                            wellNums()*2)
  analyzedEvents['Well'] <- whichNames()
  analyzedEvents <- analyzedEvents %>% dplyr::select(Well, Condition, everything())
  analyzedEvents
})

analyzedPreference <- reactive({
  preferences <- analyzePreference(analyzedEvents(), wellNums())
  preferences <- as.data.frame(do.call(rbind, preferences))
  preferences = data.frame(lapply(preferences, as.numeric))
  
  if (input$aversive == "A"){
    preferences <- preferences * -1
  }
  condition <- c(input$well1, input$well2, input$well3,
                 input$well4)
  
  if (input$flicFlea == "FLIC"){
    condition <- append(condition, c(input$well5, input$well6))
  }
  preferences['Condition'] <- condition
  preferences <- preferences %>% dplyr::select(Condition, everything())
  
  preferences
})

tidyLegBouts <- reactive({
  tidied <- tidy_bouts(removeUntilFirstEvent(editedEvents(), input$length, wellNums(), whichNames()), 'Leg')
  colnames(tidied) <- whichNames()
  tidied
})

tidyProbBouts <- reactive({
  tidied <- tidy_bouts(removeUntilFirstEvent(editedEvents(), input$length, wellNums(), whichNames()), 'Proboscis')
  colnames(tidied) <- whichNames()
  tidied
})

observe({
  shinyjs::toggleState("analyzeDataButton", !is.null(input$solutionA) && input$solutionA != "" && 
                         !is.null(input$solutionB) && input$solutionB != "")
})
goOnAnalyzeData <- eventReactive(input$analyzeDataButton, {
  input$fin
})



observeEvent(input$analyzeDataButton, {
  updateTabItems(session, "tabs", "results")
})