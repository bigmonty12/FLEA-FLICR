# analysis_ui.R
# Analysis tab of Shiny app

tabItem(tabName = "analysis",
        shinyjs::useShinyjs(),
        h2("Analysis"),
        fluidRow(
          box(
            numericInput("numTypes", "Number of genotypes/phenotypes", value = 2, min = 1),
            uiOutput("types", inline = TRUE),
            shinyjs::disabled(actionButton("describeWellsButton", "Assign Wells")),
            width = 7)
        ),
        conditionalPanel(
          condition = "input.describeWellsButton > 0",
          fluidRow(
            box(
              uiOutput("well1"),
              uiOutput("well2"),
              uiOutput("well3"),
              uiOutput("well4"),
              uiOutput("well5"),
              uiOutput("well6"),
              actionButton("inputSolutionsButton", "Input Solutions")
            )
          )
        ),
        conditionalPanel(
          condition = "input.inputSolutionsButton > 0",
          fluidRow(
            box(
              textInput("solutionA", "Solution A"),
              textInput("solutionB", "Solution B"),
              selectInput("aversive", "Aversive Compound", c("A", "B"), selected = "B"),
              numericInput("length", "How many minutes to analyze?", value=30),
              width = 5,
              shinyjs::disabled(actionButton("analyzeDataButton", "Analyze Data"))
            )
          )
        )
)