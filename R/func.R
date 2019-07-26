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
#' whether the EDA table was created using \code{'count'} or \code{'average'}
#'
#' @param analysis_result The EDA analysis result as a \code{'data.frame'}
#' @param type Can be \code{'count'} to plot the EDA resuls when calculated
#'   using counts, \code{'average'} to plot the results when calculated using
#'   the average of the target variable or \code{'count_all'} to get a count of
#'   all the values in the variables
plot_analysis_category <- function(analysis_result, type = 'count', position = 'fill') {
  var_name <- colnames(analysis_result)[1]
  target_var <- colnames(analysis_result)[2]
  if (type == 'count') {
    ggplot2::ggplot(analysis_result) +
      ggplot2::geom_col(aes_string(x = var_name, y = 'count', fill = target_var), position = position) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
  } else if (type == 'average') {
    ggplot2::ggplot(analysis_result) +
      ggplot2::geom_col(aes_string(x = var_name, y = target_var), fill = '#df0000') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
  } else if (type == 'count_all') {
    ggplot2::ggplot(analysis_result) +
      ggplot2::geom_col(aes_string(x = var_name, y = 'count'), fill = '#ec8800') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
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
