run_eda <- function(data) {
  # TODO: Add in UI options for creating the data and saving it and loading it and create an HTML with the output
  # TODO: Add better UI for the ignore or select variables

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("EDA tool"),
    miniUI::miniContentPanel(
      shiny::varSelectizeInput("targetvar", "Target variable", data),
      shiny::varSelectizeInput("ignorevars", "Ignore variables", data, multiple = TRUE),
      radioButtons("type", "Type of EDA output", choices = c("average", "count"), selected = "average")

    )
  )

  server <- function(input, output, session) {

    outputfile <- "analysis_results.RData"
    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      factor_results <- get_factor_results(data, input$targetvar, input$ignorevars, type = 'average')
      numeric_results <- get_numeric_results(data, input$targetvar, input$ignorevars, type = 'average')
      target_line <- get_target_line(data, input$targetvar)
      save(factor_results, numeric_results, target_line, file = outputfile)
      cat("Load the results using load('", outputfile, "')\n", sep = "")
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = dialogViewer("Run EDA"))
}
