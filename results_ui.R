# results_ui.R
# Results tab of Shiny app

tabItem(tabName = "results",
        h1("Results"),
        conditionalPanel(
          condition = "input.analyzeDataButton > 0",
          h2("Event Data"),
          fluidRow(
            box(
              dataTableOutput("analyzedEvents"),
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
          )
        )
)