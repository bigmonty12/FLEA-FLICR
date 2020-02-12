# results_server.R
# Results tab of Shiny app

output$analyzedEvents <- renderDataTable({
  goOnAnalyzeData()
  analyzedEvents <- analyzedEvents()
  analyzedEvents
})

output$analyzedPreference <- DT::renderDataTable({ 
  goOnAnalyzeData()
  analyzedPreference <- analyzedPreference()
  analyzedPreference
})

output$downloadAnalyzedEvents <- downloadHandler(
  filename = function(){
    file = paste0("analyzedEvents_", fileName(), ".csv")
  },
  content = function(file){
    write.csv(analyzedEvents = data.frame(lapply(analyzedEvents(), as.character), stringsAsFactors=FALSE), 
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