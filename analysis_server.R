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

#==== Remove data until first event of the arena ==== 
removeUntilFirstEvent <- function(x){
  l <- input$length
  rows <- l * 60 * 5
  df <- data.frame(matrix(0, nrow = rows, ncol = 12))
  for (i in seq(1, 11, 2)) {
    # Time of first event in arena
    m <- min(rle(x[[i]])$lengths[1], rle(x[[i+1]])$lengths[1])
    # Length of analysis min(30 minutes and remain length after removal until first event)
    l <- min(rows, length(tail(x[[i]], -m)))
    
    df[[i]] <- head(tail(x[[i]], -m), l)
    df[[i+1]] <- head(tail(x[[i+1]], -m), l)
  }
  colnames(df) <- names
  return(df)
}

timeAnalyzed <- function(x){
  rows <- length(x[[1]])
  mins <- rows / 300
  return(mins)
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
  z = 1
  for (i in seq(1, 11, 2)) {
    prob1 <- as.integer(x[i,]$Proboscis.Seconds)
    prob2 <- as.integer(x[i+1,]$Proboscis.Seconds)
    probPreference <- (prob2 - prob1) / (prob2 + prob1)
    leg1 <- as.integer(x[i,]$Leg.Seconds)
    leg2 <- as.integer(x[i+1,]$Leg.Seconds)
    legPreference <- (leg2 - leg1) / (leg2 + leg1)
    totalPreference <- ((prob2 + leg2) - (prob1 + leg1)) / ((prob2 + leg2) + (prob1 + leg1))
    events1 <- as.integer(x[i,]$Leg.Seconds)
    events2 <- as.integer(x[i+1,]$Leg.Seconds)
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
  labeledEvents[labeledEvents > 0 & labeledEvents < 100] <- 1
  labeledEvents[labeledEvents >= 100] <- 2
  labeledEvents
})

editedEvents <- reactive({
  labeledEvents <- labelEvents()
  editedEvents <- as.data.frame(lapply(labeledEvents, editEvents))
  editedEvents
})

analyzedEvents <- reactive({
  editedEvents <- editedEvents()
  removedUntilFirstEvent <- removeUntilFirstEvent(editedEvents)
  analyzedEvents <- lapply(removedUntilFirstEvent, analyzeEvents)
  analyzedEvents <- as.data.frame(do.call(rbind, analyzedEvents))
  analyzedEvents['Condition'] <- c(paste0(input$well1, ": ", input$solutionA), 
                                paste0(input$well1, ": ", input$solutionB),
                                paste0(input$well2, ": ", input$solutionA), 
                                paste0(input$well2, ": ", input$solutionB),
                                paste0(input$well3, ": ", input$solutionA), 
                                paste0(input$well3, ": ", input$solutionB),
                                paste0(input$well4, ": ", input$solutionA), 
                                paste0(input$well4, ": ", input$solutionB),
                                paste0(input$well5, ": ", input$solutionA), 
                                paste0(input$well5, ": ", input$solutionB),
                                paste0(input$well6, ": ", input$solutionA), 
                                paste0(input$well6, ": ", input$solutionB)
                                )
  analyzedEvents['Minutes.Analyzed'] <- rep(timeAnalyzed(removedUntilFirstEvent), 12)
  analyzedEvents['Well'] <- names
  analyzedEvents <- analyzedEvents %>% dplyr::select(Well, Condition, everything())
  analyzedEvents
})

analyzedPreference <- reactive({
  analyzedEvents <- analyzedEvents()
  preferences <- analyzePreference(analyzedEvents)
  preferences <- as.data.frame(do.call(rbind, preferences))
  preferences = data.frame(lapply(preferences, as.numeric))
  
  if (input$aversive == "A"){
    preferences <- preferences * -1
  }
  preferences['Condition'] <- c(input$well1, input$well2, input$well3,
                                input$well4, input$well5, input$well6)
  preferences <- preferences %>% dplyr::select(Condition, everything())
  
  preferences
})

goOnAnalyzeData <- eventReactive(input$analyzeDataButton, {
  input$fin
})



observeEvent(input$analyzeDataButton, {
  updateTabItems(session, "tabs", "results")
})