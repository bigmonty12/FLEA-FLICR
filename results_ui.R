# results_ui.R
# Results tab of Shiny app

tabItem(tabName = "results",
        h1("Results"),
        conditionalPanel(
          condition = "input.analyzeDataButton > 0",
          h2("Event Data"),
          fluidRow(
            box(
              DT::dataTableOutput("analyzedEvents"),
              downloadButton("downloadAnalyzedEvents", "Download Analyzed Events"),
              width = 12
            )
          ),
          h2("Preference Data"),
          fluidRow(
            box(
              DT::dataTableOutput("analyzedPreference"),
              downloadButton("downloadAnalyzedPreference", "Download Analyzed Preference"),
              
              width = 12
            )
          ),
          h2("Leg Event Lengths (seconds)"),
          fluidRow(
            box(
              DT::dataTableOutput("legBouts"),
              downloadButton("downloadLegBouts", "Download Leg Event Lengths"),
              width = 12
            )
          ),
          h2("Proboscis Event Lengths (seconds)"),
          fluidRow(
            box(
              DT::dataTableOutput("probBouts"),
              downloadButton("downloadProbBouts", "Download Proboscis Event Lengths"),
              width = 12
            )
          )
        )
)