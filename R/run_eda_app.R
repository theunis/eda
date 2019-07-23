run_eda <- function(data = NULL) {

  datasets <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("EDA tool"),
    miniUI::miniContentPanel(
      shiny::selectizeInput("dataset", "Select dataset", datasets),
      shiny::checkboxInput("buildnew", "Build new EDA dataset", value = TRUE),
      shiny::conditionalPanel(
        condition = "input.buildnew == true",
        shiny::selectInput("targetvar", "Target variable", colnames(data)),
        shiny::fillRow(
          shiny::selectInput("ignorevars", "Ignore variables", colnames(data), multiple = TRUE, size = 8, selectize = FALSE),
          shiny::fluidRow(
            shiny::column(12, align = "center",
                   shiny::actionButton("addtovars", "", icon = shiny::icon("chevron-right"), width = "60px"),
                   shiny::actionButton("removefromvars", "", icon = shiny::icon("chevron-left"), width = "60px")
            )
          ),
          shiny::selectInput("selectedvars", "Selected variables", c(), multiple = TRUE, size = 8, selectize = FALSE),
          flex = c(5, 2, 5),
          height = "200px"
        ),
        shiny::radioButtons("type", "Type of EDA output", choices = c("average", "count"), selected = "average")
      ),
      shiny::checkboxInput("buildrmd", "Build Rmd file", value = TRUE),
      miniUI::conditionPanel(
        condition = "input.buildrmd == true",
        shiny::checkboxInput("openhtml", "Open output HTML file", value = TRUE)
      )
    )
  )

  server <- function(input, output, session) {

    data_variables <- reactive({
      colnames(eval(rlang::sym(input$dataset)))
    })

    shiny::observe({
      # dataset <- input$dataset
      # data_variables <<- colnames(eval(rlang::sym(dataset)))
      updateSelectInput(session, "targetvar", choices = data_variables())
      selected_vars <<- c()
      updateSelectInput(session, "selectedvars", choices = c())
      ignored_vars <<- data_variables()
      updateSelectInput(session, "ignorevars", choices = data_variables())
    })

    # selected_vars <- c()
    # ignored_vars <- data_variables()

    shiny::observeEvent(input$addtovars, {
      selected_vars <<- c(selected_vars, input$ignorevars)
      updateSelectInput(session, "selectedvars", choices = selected_vars)
      ignored_vars <<- data_variables()[!(data_variables() %in% selected_vars)]
      updateSelectInput(session, "ignorevars", choices = ignored_vars)
    })

    shiny::observeEvent(input$removefromvars, {
      ignored_vars <<- c(ignored_vars, input$selectedvars)
      updateSelectInput(session, "ignorevars", choices = ignored_vars)
      selected_vars <<- data_variables()[!(data_variables() %in% ignored_vars)]
      updateSelectInput(session, "selectedvars", choices = selected_vars)
    })

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      if (input$buildnew) {
        factor_results <- get_factor_results(data, input$targetvar, ignored_vars, type = 'average')
        numeric_results <- get_numeric_results(data, input$targetvar, ignored_vars, type = 'average')
        target_line <- get_target_line(data, input$targetvar)
        outputfile <- paste0(getwd(), "/analysis_results", input$dataset, ".RData")

        save(factor_results, numeric_results, target_line, file = outputfile)
        cat("Load the results using load('", outputfile, "')\n", sep = "")
      }
      if (input$buildrmd) {
        eda_rmdfile <- system.file("rmarkdown/templates/Exploratory Data Analysis/skeleton", "skeleton.Rmd", package = "eda")
        rmdfile <- paste0(getwd(), '/EDA_', input$dataset, '.html')
        rmarkdown::render(eda_rmdfile, output_file = rmdfile,
                          params = list(analysis_results = outputfile,
                                        type = input$type))
        if (input$openrmd) {
          browseURL(file.path("file:/", rmdfile))
        }
      }
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Run EDA"))
}
