# analysis_server.R
# Analysis tab of Shiny app

findNumTypes <- reactive({
  numTypes <- as.integer(input$numTypes)
})

output$types <- renderUI({
  numTypes <- findNumTypes()
  lapply(1:numTypes, function(i) {
    textInput(inputId = paste0("type", i), 
              label = paste("Genotype/Phenotype #", i))
  })
})

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

output$well1 <- renderUI({
  types <- numTypesList()
  selectInput("well1", "Well 1", choices = types)
})

output$well2 <- renderUI({
  types <- numTypesList()
  selectInput("well2", "Well 2", choices = types)
})

output$well3 <- renderUI({
  types <- numTypesList()
  selectInput("well3", "Well 3", choices = types)
})

output$well4 <- renderUI({
  types <- numTypesList()
  selectInput("well4", "Well 4", choices = types)
})

output$well5 <- renderUI({
  types <- numTypesList()
  selectInput("well5", "Well 5", choices = types)
})

output$well6 <- renderUI({
  types <- numTypesList()
  selectInput("well6", "Well 6", choices = types)
})