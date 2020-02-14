# preprocessing_ui.R
# Preprocessing tab of Shiny app

tabItem(tabName = "preprocessing",
        waiter::use_waiter(),
        fluidRow(
          box(fileInput("fin", "Select Raw FLIC file", accept = c('text/csv', '.csv')),
              actionButton("submitButton", "Submit and View Wells"),
              width = 5),
          box(selectInput("baselineMethod", "Choose Baseline Method",
                          choices = c("Running Median (fast, less precise)",
                                      "EEG (slow, more precise)"),
                          selected = "Running Median (fast, less precise)"),
              width = 5)),
        conditionalPanel(
          condition = "input.submitButton > 0 & input.findButton == 0",
          h2("Raw Plots"),
          fluidRow(
            box(plotOutput("rawPlots"), width = 12,
                actionButton("findButton", "Find Baseline"),
                downloadButton("downloadRawPlots", "Download Plots")))
        ),
        conditionalPanel(
          condition = "input.findButton > 0 & input.subtractButton == 0",
          h2("Baseline Drawn"),
          fluidRow(
            box(plotOutput("baselinePlots"), width = 12,
                actionButton("subtractButton", "Subtract Baseline"),
                downloadButton("downloadBaseline", "Download Plots")))),
        conditionalPanel(
          condition = "input.subtractButton > 0 & input.removeBlipsButton == 0",
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
                actionButton("selectWellsButton", "Go to Analysis"),
                downloadButton("downloadRemovedBlips", "Download Plots"))
          )
        )
)