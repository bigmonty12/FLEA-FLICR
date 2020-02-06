# analysis_ui.R
# Analysis tab of Shiny app

tabItem(tabName = "analysis",
        h2("Analysis"),
        fluidRow(
          box(
            numericInput("numTypes", "Number of genotypes/phenotypes", value = 2),
            uiOutput("types", inline = TRUE),
            actionButton("describeWellsButton", "Assign Wells"),
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
              width = 5,
              actionButton("analyzeDataButton", "Analyze Data")
            )
          )
        )
)