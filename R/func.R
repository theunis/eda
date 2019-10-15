#' Create EDA table from dataset
#'
#' These functions create tables summarizing how the variable \code{var_name} in
#' the dataset \code{df} relates to the specified target variable
#' \code{target_var}.
#'
#' You can specify the \code{type} as \code{'count'} or \code{'average'}. Use
#' \code{'count'} to get counts of each variable value for the values of the
#' target variable. Use \code{'average'} to get the average of the target
#' variable for variable value.
#'
#' In the case of \code{create_numeric_table} the input variable is a numeric
#' variable and its values are created as bins, with number of bins defined as
#' \code{bins}
#'
#' @param df A \code{data.frame} with the dataset to analyze
#' @param var_name A string with the variable name to analyze
#' @param target_var A string with the name of the target variable
#' @param type Can be \code{'count'} or \code{'average'} depicting how to get
#'   the results back
#' @param bins Number of bins to use for numeric variables
create_analysis_table <- function(df, var_name, target_var, type = 'count') {
  # assertthat::assert_that(length(unique(df[,target_var])[[1]]) == 2)
  if (type == 'count') {
    result <- df %>%
      dplyr::group_by(!!rlang::sym(var_name), !!rlang::sym(target_var)) %>%
      dplyr::summarise(count = dplyr::n())
  } else if (type == 'average') {
    result <- df %>%
      dplyr::group_by(!!rlang::sym(var_name)) %>%
      dplyr::summarise(!!rlang::sym(target_var) := mean(!!rlang::sym(target_var), na.rm = TRUE),
                count = dplyr::n())
  }

  result
}

#' @rdname create_analysis_table
create_numeric_table <- function(df, var_name, target_var, type = 'count', bins = 30) {
  # assertthat::assert_that(length(unique(df[,target_var])[[1]]) == 2)
  if (type == 'count') {
    result <- df %>%
      dplyr::filter(!is.na(!!rlang::sym(var_name))) %>%
      dplyr::group_by(!!rlang::sym(var_name) := !!dbplot::db_bin(!!rlang::sym(var_name), bins = bins),
               !!rlang::sym(target_var)) %>%
      dplyr::summarise(count = dplyr::n())
  } else if (type == 'average') {
    result <- df %>%
      dplyr::filter(!is.na(!!rlang::sym(var_name))) %>%
      dplyr::group_by(!!rlang::sym(var_name) := !!dbplot::db_bin(!!rlang::sym(var_name), bins = bins)) %>%
      dplyr::summarise(!!rlang::sym(target_var) := mean(!!rlang::sym(target_var), na.rm = TRUE),
                count = dplyr::n())
  }

  result
}

#' Plot EDA tables
#'
#' Create plots with all variables in the EDA table created using
#' \code{create_analysis_table} or \code{create_numeric_table}.
#'
#' Use the right type (\code{'count'} or \code{'average'}}), depending on
#' whether the EDA table was created using \code{'count'} or \code{'average'}.
#'
#' For Vega-Lite plots use the function \code{plot_analysis_category_vl}.
#'
#' @param analysis_result The EDA analysis result as a \code{'data.frame'}
#' @param type Can be \code{'count'} to plot the EDA resuls when calculated
#'   using counts, \code{'average'} to plot the results when calculated using
#'   the average of the target variable or \code{'count_all'} to get a count of
#'   all the values in the variables
#' @param position When \code{type = 'count'}, what position parameter to use
#' @param color The fill color of the bars
plot_analysis_category <- function(analysis_result, type = 'count', position = 'fill', color = '#df0000') {
  var_name <- colnames(analysis_result)[1]
  target_var <- colnames(analysis_result)[2]
  if (type == 'count') {
    ggplot2::ggplot(analysis_result) +
      ggplot2::geom_col(aes_string(x = var_name, y = 'count', fill = target_var), position = position) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggplot2::theme_minimal()
  } else if (type == 'average') {
    ggplot2::ggplot(analysis_result) +
      ggplot2::geom_col(aes_string(x = var_name, y = target_var), fill = color) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggplot2::theme_minimal()
  } else if (type == 'count_all') {
    ggplot2::ggplot(analysis_result) +
      ggplot2::geom_col(aes_string(x = var_name, y = 'count'), fill = color) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggplot2::theme_minimal()
  }
}

#' Get factor/numeric variables in a \code{data.frame}
#'
#' Gets a character vector with all the factor/numeric variables in the specified
#' \code{data.frame} df.
#'
#' @param df The input \code{data.frame}
#' @param target_var A string with the name of the target variable
#' @param ignore_vars A character vector with the variables to ignore
get_factor_vars <- function(df, target_var, ignore_vars = c()) {

  col_names <- names(df)
  is_factor_var <- df %>%
    purrr::map(class) %>%
    purrr::map_lgl(~any(. %in% c('factor', 'logical', 'character')))
  factor_vars <- col_names[which(is_factor_var)]
  factor_vars <- factor_vars[!(factor_vars %in% c(target_var, ignore_vars))]
  factor_vars
}

#' @rdname get_factor_vars
get_numeric_vars <- function(df, target_var, ignore_vars = c()) {

  col_names <- names(df)
  is_numeric_var <- df %>%
    purrr::map(class) %>%
    purrr::map_lgl(~any(. %in% c('numeric', 'integer')))
  numeric_vars <- col_names[which(is_numeric_var)]
  numeric_vars <- numeric_vars[!(numeric_vars %in% c(target_var, ignore_vars))]
  numeric_vars
}

#' @rdname plot_analysis_category
plot_analysis_category_vl <- function(analysis_result, type = 'average', color = 'salmon') {
  variable_name <- colnames(analysis_result)[1]
  if (stringr::str_detect(variable_name, '\\.')) {
    variable_name <- stringr::str_replace_all(variable_name, '\\.', '_')
    colnames(analysis_result)[1] <- variable_name
  }
  target_var <- colnames(analysis_result)[2]
  target_line <- sum(analysis_result['count']*analysis_result[target_var]) / sum(analysis_result['count'])

  if (type == 'average') {
    barchart <- vlbuildr::vl_chart() %>%
      vlbuildr::vl_add_data(values = analysis_result) %>%
      vlbuildr::vl_encode_x(variable_name, "nominal") %>%
      vlbuildr::vl_encode_y(target_var, "quantitative") %>%
      vlbuildr::vl_mark_bar(color = color)

    tl_analysis_result <- analysis_result
    tl_analysis_result[target_var] <- target_line

    target_line_chart <- vlbuildr::vl_chart() %>%
      vlbuildr::vl_add_data(values = tl_analysis_result) %>%
      vlbuildr::vl_encode_y(target_var, "quantitative") %>%
      vlbuildr::vl_mark_rule(size = 2)

    return(vlbuildr::vl_layer(barchart, target_line_chart))
  } else if (type == 'count_all') {
    barchart <- vlbuildr::vl_chart() %>%
      vlbuildr::vl_add_data(values = analysis_result) %>%
      vlbuildr::vl_encode_x(variable_name, "nominal") %>%
      vlbuildr::vl_encode_y("count", "quantitative") %>%
      vlbuildr::vl_mark_bar(color = color)
    return(barchart)
  }
}

#' Plot all EDA tables using Vega-Lite
#'
#' Create plots with all variables in the EDA table created using
#' \code{get_factor_results} or \code{get_numeric_results}.
#'
#' @param analysis_results The EDA analysis results as a \code{'data.frame'}
#' @param which Which plots to show, can be \code{'count'} plotting the
#'   histograms, \code{'average'} plotting the averages of the target variable,
#'   or \code{'both'} plotting both averages and histograms.
#' @param color_avg,color_count The fill color of the bars for averages and
#'   counts.
plot_all_analysis_results_vl <- function(analysis_results, which = 'both', color_avg = 'salmon', color_count = 'skyblue') {
  avg_lst <- purrr::map(analysis_results$analysis_table, plot_analysis_category_vl, color = color_avg)
  count_lst <- purrr::map(analysis_results$analysis_table, plot_analysis_category_vl, type = 'count_all', color = color_count)

  switch (which,
          'both' = {
            lst <- append(avg_lst, count_lst)
            lst[['columns']] <- length(avg_lst)
            do.call(vlbuildr::vl_concat, lst)
          },
          'average' = {
            avg_lst[['columns']] <- length(avg_lst)
            do.call(vlbuildr::vl_concat, avg_lst)
          },
          'count' = {
            count_lst[['columns']] <- length(count_lst)
            do.call(vlbuildr::vl_concat, count_lst)
          }
  )

}


