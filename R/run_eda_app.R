run_eda <- function(data = NULL) {

  assertthat::assert_that(
    !is.null(unlist(eapply(.GlobalEnv,is.data.frame))),
    msg = 'No data.frame found, load a data.frame in the environment before running the tool'
  )
  datasets <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
  assertthat::assert_that(length(datasets) > 0)

  if (!is.null(data)) {
    selected_dataset <- deparse(substitute(data))
  } else {
    selected_dataset <- datasets[1]
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("EDA tool"),
    miniUI::miniContentPanel(
      shiny::selectizeInput("dataset", "Select dataset", datasets,
                            selected = selected_dataset),
      shiny::checkboxInput("buildnew", "Build new EDA dataset", value = TRUE),
      shiny::conditionalPanel(
        condition = "input.buildnew == true",
        shiny::selectInput("targetvar", "Target variable", c()),
        shiny::fillRow(
          shiny::selectInput("ignorevars", "Ignore variables", c(),
                             multiple = TRUE, size = 8, selectize = FALSE),
          shiny::fluidRow(
            shiny::column(12, align = "center",
                   shiny::actionButton("addtovars", "",
                                       icon = shiny::icon("chevron-right"),
                                       width = "60px"),
                   shiny::actionButton("removefromvars", "",
                                       icon = shiny::icon("chevron-left"),
                                       width = "60px")
            )
          ),
          shiny::selectInput("selectedvars", "Selected variables", c(),
                             multiple = TRUE, size = 8, selectize = FALSE),
          flex = c(5, 2, 5),
          height = "200px"
        ),
        shiny::radioButtons("type", "Type of EDA output",
                            choices = c("average", "count"),
                            selected = "average")
      ),
      shiny::checkboxInput("writecsv", "Write CSV file", value = FALSE),
      shiny::checkboxInput("buildrmd", "Build Rmd file", value = TRUE),
      shiny::conditionalPanel(
        condition = "input.buildrmd == true",
        shiny::checkboxInput("openhtml", "Open output HTML file", value = TRUE)
      )
    )
  )

  server <- function(input, output, session) {

    data <- reactive({
      eval(rlang::sym(input$dataset))
    })

    data_variables <- reactive({
      colnames(data())
    })

    shiny::observe({
      outputfile <- paste0(getwd(), "/analysis_results-",
                           input$dataset, ".RData")
      target_var <- get_output_target(outputfile)
      updateSelectInput(session, "targetvar", choices = data_variables(),
                        selected = target_var)
      selected_vars <<- get_output_selectedvars(outputfile)
      # print(selected_vars)
      # selected_vars <<- c()
      updateSelectInput(session, "selectedvars", choices = selected_vars)
      ignored_vars <<- data_variables()[!(data_variables() %in% selected_vars)]
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
        factor_results <- get_factor_results(data(), input$targetvar,
                                             ignored_vars, type = 'average')
        numeric_results <- get_numeric_results(data(), input$targetvar,
                                               ignored_vars, type = 'average')
        target_line <- get_target_line(data(), input$targetvar)
        outputfile <- paste0(getwd(),
                             "/analysis_results-", input$dataset, ".RData")

        save(factor_results, numeric_results, target_line, file = outputfile)
        cat("Load the results using load('", outputfile, "')\n", sep = "")
      }
      if (input$writecsv) {
        outputfile <- paste0(getwd(),
                             "/analysis_results-", input$dataset, ".RData")
        outputfile_csv <- paste0(getwd(),
                             "/analysis_results-", input$dataset, ".csv")
        load(outputfile)
        res <- dplyr::bind_rows(factor_results, numeric_results)
        res$analysis_table <- res$analysis_table %>%
          map(rename_colname_to_value, 'value')
        res %>%
          tidyr::unnest(analysis_table) %>%
          dplyr::mutate(average = target_line) %>%
          readr::write_csv(path = outputfile_csv)
      }
      if (input$buildrmd) {
        eda_rmdfile <- system.file(
          "rmarkdown/templates/Exploratory Data Analysis/skeleton",
          "skeleton.Rmd", package = "eda"
          )
        rmdfile <- paste0(getwd(), '/EDA-', input$dataset, '.html')
        rmarkdown::render(eda_rmdfile, output_file = rmdfile,
                          params = list(analysis_results = outputfile,
                                        type = input$type))
        if (input$openhtml) {
          browseURL(file.path("file:/", rmdfile))
        }
      }
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Run EDA"))
}

get_output_selectedvars <- function(analysis_results_file) {
  if (file.exists(analysis_results_file)) {
    load(analysis_results_file)
  } else {
    out <- NULL
    return(out)
  }
  # check that the right data.frames are loaded:
  assertthat::assert_that(exists('factor_results') &&
                            is.data.frame(get('factor_results')))
  assertthat::assert_that(exists('numeric_results') &&
                            is.data.frame(get('numeric_results')))

  out <- c(factor_results$variable, numeric_results$variable)
  remove(factor_results, numeric_results)

  out
}

get_output_target <- function(analysis_results_file) {
  if (file.exists(analysis_results_file)) {
    load(analysis_results_file)
  } else {
    out <- NULL
    return(out)
  }
  # check that the right data.frames are loaded:
  assertthat::assert_that(exists('factor_results') &&
                            is.data.frame(get('factor_results')))
  assertthat::assert_that(exists('numeric_results') &&
                            is.data.frame(get('numeric_results')))

  if (!(length(factor_results$analysis_table) == 0)) {
    out <- colnames(factor_results$analysis_table[[1]])[2]
  } else if (!(length(numeric_results$analysis_table) == 0)) {
    out <- colnames(numeric_results$analysis_table[[1]])[2]
  } else {
    out <- NULL
  }
  out
}

rename_colname_to_value <- function(x, y) {
  colnames(x)[1] <- y
  x
}
