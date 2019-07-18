run_eda <- function(data) {
  # TODO: Add better UI for the ignore or select variables

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("EDA tool"),
    miniUI::miniContentPanel(
      shiny::checkboxInput("buildnew", "Build new EDA dataset", value = TRUE),
      shiny::conditionalPanel(
        condition = "input.buildnew == true",
        shiny::selectInput("targetvar", "Target variable", colnames(data)),
        shiny::selectInput("ignorevars", "Ignore variables", colnames(data), multiple = TRUE, size = 8, selectize = FALSE),
        shiny::radioButtons("type", "Type of EDA output", choices = c("average", "count"), selected = "average")
      ),
      shiny::checkboxInput("buildrmd", "Build Rmd file", value = TRUE)

    )
  )

  server <- function(input, output, session) {

    outputfile <- paste0(getwd(), "/analysis_results.RData")
    rmdfile <- paste0(getwd(), '/EDA.html')
    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      if (input$buildnew) {
        factor_results <- get_factor_results(data, input$targetvar, input$ignorevars, type = 'average')
        numeric_results <- get_numeric_results(data, input$targetvar, input$ignorevars, type = 'average')
        target_line <- get_target_line(data, input$targetvar)
        save(factor_results, numeric_results, target_line, file = outputfile)
        cat("Load the results using load('", outputfile, "')\n", sep = "")
      } else {
        eda_rmdfile <- system.file("rmarkdown/templates/Exploratory Data Analysis/skeleton", "skeleton.Rmd", package = "eda")
        rmarkdown::render(eda_rmdfile, output_file = rmdfile,
                          params = list(analysis_results = outputfile,
                                        type = input$type))
        browseURL(file.path("file:/", rmdfile))
      }
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Run EDA"))
}
