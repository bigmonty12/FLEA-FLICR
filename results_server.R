# results_server.R
# Results tab of Shiny app

output$analyzedEvents <- DT::renderDataTable({
  goOnAnalyzeData()
  analyzedEvents <- analyzedEvents()
  print(pryr::mem_used())
  analyzedEvents
})

output$analyzedPreference <- DT::renderDataTable({ 
  goOnAnalyzeData()
  analyzedPreference <- analyzedPreference()
  print(pryr::mem_used())
  analyzedPreference
})

bouts <- function(df, type){
  if(type == 'Leg'){
    x <- 1
  }
  if(type == 'Proboscis'){
    x <- 2
  }
  r <- rle(df)
  return(r$lengths[which(r$values == x)])
}

output$legBouts <- DT::renderDataTable({
  goOnAnalyzeData()
  cleanData <- removeUntilFirstEvent(editedEvents())
  #col_names <- c("1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A", "5B", "6A", "6B")
  x <- lapply(cleanData, bouts, 'Leg')
  cleanData <- t(plyr::ldply(x, rbind, .id=NULL))
  if(input$flicFlea == 'FLIC'){
    colnames(cleanData) <- namesFLIC
  } else{
    colnames(cleanData) <- namesFLEA
  }
  cleanData / 5
})

output$probBouts <- DT::renderDataTable({
  goOnAnalyzeData()
  cleanData <- removeUntilFirstEvent(editedEvents())
  x <- lapply(cleanData, bouts, 'Proboscis')
  cleanData <- t(plyr::ldply(x, rbind, .id=NULL))
  if(input$flicFlea == 'FLIC'){
    colnames(cleanData) <- namesFLIC
  } else{
    colnames(cleanData) <- namesFLEA
  }
  cleanData / 5
})

output$downloadAnalyzedEvents <- downloadHandler(
  filename = function(){
    file = paste0("analyzedEvents_", fileName(), ".csv")
  },
  content = function(file){
    write.csv(data.frame(lapply(analyzedEvents(), as.character), stringsAsFactors=FALSE), 
              file)
  }
)

output$downloadAnalyzedPreference <- downloadHandler(
  filename = function(){
    file = paste0("analyzedPreference_", fileName(), ".csv")
  },
  content = function(file){
    write.csv(data.frame(lapply(analyzedPreference(), as.character), stringsAsFactors=FALSE), 
              file)
  }
)

output$downloadLegBouts <- downloadHandler(
  filename = function(){
    file = paste0("legBouts_", fileName(), ".csv")
  },
  content = function(file){
    goOnAnalyzeData()
    cleanData <- removeUntilFirstEvent(editedEvents())
    x <- lapply(cleanData, bouts, 'Leg')
    cleanData <- t(plyr::ldply(x, rbind, .id=NULL))
    if(input$flicFlea == 'FLIC'){
      colnames(cleanData) <- namesFLIC
    } else{
      colnames(cleanData) <- namesFLEA
    }
    write.csv(cleanData / 5, file)
  }
)

output$downloadProbBouts <- downloadHandler(
  filename = function(){
    file = paste0("probBouts_", fileName(), ".csv")
  },
  content = function(file){
    goOnAnalyzeData()
    cleanData <- removeUntilFirstEvent(editedEvents())
    x <- lapply(cleanData, bouts, 'Proboscis')
    cleanData <- t(plyr::ldply(x, rbind, .id=NULL))
    if(input$flicFlea == 'FLIC'){
      colnames(cleanData) <- namesFLIC
    } else{
      colnames(cleanData) <- namesFLEA
    }
    write.csv(cleanData / 5, file)
  }
)