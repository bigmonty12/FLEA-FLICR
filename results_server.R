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

output$legBouts <- DT::renderDataTable({
  goOnAnalyzeData()
  tidyBouts <- tidyLegBouts()
  tidyBouts 
})

output$probBouts <- DT::renderDataTable({
  goOnAnalyzeData()
  tidyBouts <- tidyProbBouts()
  tidyBouts 
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

output$downloadAllEventsAndTime <- downloadHandler(
  filename = function(){
    file = paste0("eventsWithTime_", fileName(), ".csv")
  },
  content = function(file){
    write.csv(wellEncodedTimeEvents(), quote = F, row.names = F, file)
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
    tidyBouts <- tidyLegBouts()
    if(input$flicFlea == 'FLIC'){
      colnames(tidyBouts) <- namesFLIC
    } else{
      colnames(tidyBouts) <- namesFLEA
    }
    write.csv(tidyBouts, file)
  }
)

output$downloadProbBouts <- downloadHandler(
  filename = function(){
    file = paste0("probBouts_", fileName(), ".csv")
  },
  content = function(file){
    goOnAnalyzeData()
    tidyBouts <- tidyProbBouts()
    if(input$flicFlea == 'FLIC'){
      colnames(tidyBouts) <- namesFLIC
    } else{
      colnames(tidyBouts) <- namesFLEA
    }
    write.csv(tidyBouts, file)
  }
)