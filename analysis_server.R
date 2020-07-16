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

#==== Remove data until first event of the arena ==== 
removeUntilFirstEvent <- function(x){
  rows <- input$length * 60 * 5
  rowNum <- wellNums() * 2 - 1
  df <- list()
  for (i in seq(1, rowNum, 2)) {
    # Time of first event in arena
    m <- min(rle(x[[i]])$lengths[1], rle(x[[i+1]])$lengths[1])
    # Length of analysis min(30 minutes and remain length after removal until first event)
    l <- min(rows, length(tail(x[[i]], -m)))
    
    df[[i]] <- head(tail(x[[i]], -m), l)
    df[[i+1]] <- head(tail(x[[i+1]], -m), l)
  }
  df <- data.frame(lapply(df, "length<-", max(lengths(df))))
  df <- data.frame(df)
  colnames(df) <- whichNames()
  return(df)
}

timeAnalyzed <- function(x){
  return(length(x[[1]]) / 300)
}

analyzeEvents <- function(x) {
  legEventIndex <- which(rle(x)$values == 1)
  legEventNums <- length(legEventIndex)
  legEventSecs <- sum(rle(x)$lengths[legEventIndex]) / 5
  probEventIndex <- which(rle(x)$values == 2)
  probEventNums <- length(probEventIndex)
  probEventSecs <- sum(rle(x)$lengths[probEventIndex]) / 5
  totalEventNums <- legEventNums + probEventNums
  legAveEventSecs <- legEventSecs / legEventNums
  probAveEventSecs <- probEventSecs / probEventNums
  legEventPercentage <- legEventNums / totalEventNums
  eventsAnalyzed <- list("Leg.Events" = legEventNums,
                         "Proboscis.Events" = probEventNums,
                         "Total.Events" = totalEventNums,
                         "Leg.Percentage" = legEventPercentage,
                         "Leg.Seconds" = legEventSecs,
                         "Leg.Average.Seconds" = legAveEventSecs,
                         "Proboscis.Seconds" = probEventSecs,
                         "Proboscis.Average.Seconds" = probAveEventSecs
  )
  return(eventsAnalyzed)
}

analyzePreference <- function(x) {
  totalPreferences <- list()
  end <- wellNums() * 2 - 1
  z = 1
  for (i in seq(1, end, 2)) {
    prob1 <- as.integer(x[i,]$Proboscis.Seconds)
    prob2 <- as.integer(x[i+1,]$Proboscis.Seconds)
    probPreference <- (prob2 - prob1) / (prob2 + prob1)
    leg1 <- as.integer(x[i,]$Leg.Seconds)
    leg2 <- as.integer(x[i+1,]$Leg.Seconds)
    legPreference <- (leg2 - leg1) / (leg2 + leg1)
    totalPreference <- ((prob2 + leg2) - (prob1 + leg1)) / ((prob2 + leg2) + (prob1 + leg1))
    events1 <- as.integer(x[i,]$Total.Events)
    events2 <- as.integer(x[i+1,]$Total.Events)
    totalEventPreference <- (events2 - events1) / (events2 + events1)
    preferences <- list("Proboscis.Preference" = probPreference,
                        "Leg.Preference" = legPreference,
                        "Total.Preference" = totalPreference,
                        "Event.Preference" = totalEventPreference)
    totalPreferences[[z]] <- preferences
    z <- z + 1
  }
  return(totalPreferences)
} 

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

analyzedEvents <- reactive({
  analyzedEvents <- lapply(removeUntilFirstEvent(editedEvents()), analyzeEvents)
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
  analyzedEvents['Minutes.Analyzed'] <- rep(timeAnalyzed(removeUntilFirstEvent(editedEvents())), 
                                            wellNums()*2)
  analyzedEvents['Well'] <- whichNames()
  analyzedEvents <- analyzedEvents %>% dplyr::select(Well, Condition, everything())
  analyzedEvents
})

analyzedPreference <- reactive({
  preferences <- analyzePreference(analyzedEvents())
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