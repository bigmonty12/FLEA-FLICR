# preprocessing_ui.R
# Preprocessing tab of Shiny app

tabItem(tabName = "preprocessing",
        fluidRow(
          box(fileInput("fin", "Select Raw FLIC file", accept = c('text/csv', '.csv')),
              actionButton("submitButton", "Submit and View Wells"),
              width = 5)),
        conditionalPanel(
          condition = "input.submitButton > 0",
          h2("Raw Plots"),
          fluidRow(
            box(plotOutput("rawPlots"), width = 12,
                actionButton("findButton", "Find Baseline"),
                downloadButton("downloadRawPlots", "Download Plots")))
        ),
        conditionalPanel(
          condition = "input.findButton > 0",
          h2("Baseline Drawn"),
          fluidRow(
            box(plotOutput("baselinePlots"), width = 12,
                actionButton("subtractButton", "Subtract Baseline"),
                downloadButton("downloadBaseline", "Download Plots")))),
        conditionalPanel(
          condition = "input.subtractButton > 0",
          h2("Baseline Subtracted"),
          fluidRow(
            box(plotOutput("subtractBaselinePlots"), width = 12,
                actionButton("removeBlipsButton", "Remove Blips"),
                downloadButton("downloadSubtracted", "Download Plots")))),
        conditionalPanel(
          condition = "input.removeBlipsButton > 0",
          h2("Blips Removed"),
          fluidRow(
            box(plotOutput("removedBlipsPlots"), width = 12,
                downloadButton("downloadRemovedBlips", "Download Plots"))
          ),
          fluidRow(
            box(checkboxGroupInput("whichWells", "Wells to keep for analysis:",
                                   names, selected = names, inline = TRUE),
                actionButton("selectWellsButton", "Select Wells and Go to Analysis"),
                width = 12
            )
          )))