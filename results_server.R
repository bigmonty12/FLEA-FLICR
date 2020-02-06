# results_server.R
# Results tab of Shiny app

output$analyzedEvents <- renderDataTable({
  goOnAnalyzeData()
  analyzedEvents <- analyzedEvents()
  #analyzedEvents <- arrange(analyzedEvents, Condition)
  analyzedEvents
})

output$analyzedPreference <- renderDataTable({
  goOnAnalyzeData()
  analyzedPreference <- analyzedPreference()
  #analyzedPreference <- arrange(analyzedPreference, Condition)
  analyzedPreference
})

output$downloadAnalyzedEvents <- downloadHandler(
  filename = function(){
    file = paste0("analyzedEvents_", fileName(), ".csv")
  },
  content = function(file){
    analyzedEvents <- analyzedEvents()
    analyzedEvents = data.frame(lapply(analyzedEvents, as.character), stringsAsFactors=FALSE)
    write.csv(analyzedEvents, file)
  }
)

output$downloadAnalyzedPreference <- downloadHandler(
  filename = function(){
    file = paste0("analyzedPreference_", fileName(), ".csv")
  },
  content = function(file){
    analyzedPreference <- analyzedPreference()
    analyzedPreference = data.frame(lapply(analyzedPreference, as.character), stringsAsFactors=FALSE)
    write.csv(analyzedPreference, file)
  }
)