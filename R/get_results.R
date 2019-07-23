#' Create EDA data for variables in a dataset
#'
#' Get the EDA output with all the results for the factor or numeric variables
#' that are in the dataset \code{df}
#'
#' @param df The input \code{data.frame}
#' @param target_var A string with the name of the target variable
#' @param ignore_vars A character vector with the variables to ignore
#' @param type Can be \code{'count'} or \code{'average'} depicting how to get
#'   the results back
#' @param bins Number of bins to use for numeric variables
get_factor_results <- function(df, target_var, ignore_vars = c(),
                               type = 'average') {
  factor_vars <- get_factor_vars(df, target_var, ignore_vars = ignore_vars)
  factor_results <- tibble::tibble(variable = factor_vars) %>%
    dplyr::group_by(variable) %>%
    dplyr::do(analysis_table = create_analysis_table(df, var_name = .$variable,
                                                     target_var = target_var,
                                                     type = type))
  factor_results
}

#' @rdname get_factor_results
get_numeric_results <- function(df, target_var, ignore_vars = c(),
                                type = 'average', bins = 30) {
  numeric_vars <- get_numeric_vars(df, target_var, ignore_vars = ignore_vars)
  numeric_results <- tibble::tibble(variable = numeric_vars) %>%
    dplyr::group_by(variable) %>%
    dplyr::do(analysis_table = create_numeric_table(df, var_name = .$variable,
                                                    target_var = target_var,
                                                    type = type, bins = bins))
  numeric_results
}

#' Get the target variable line
#'
#' Simple function which gets the mean of the specified target variable in the
#' \code{data.frame} df. This is used to overlay on EDA plots.
#'
#' @param df The input \code{data.frame}
#' @param target_var A string with the name of the target variable
get_target_line <- function(df, target_var) {
  target_line <- df %>%
    dplyr::pull(!!rlang::sym(target_var)) %>%
    as.numeric() %>%
    mean()
  target_line
}
