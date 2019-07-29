plot_analysis_category_vl <- function(analysis_result, type = 'average') {
  variable_name <- colnames(analysis_result)[1]
  target_var <- colnames(analysis_result)[2]

  if (type == 'average') {
    barchart <- vlbuildr::vl_chart() %>%
      vlbuildr::vl_add_data(values = analysis_result) %>%
      vlbuildr::vl_encode_x(variable_name, "nominal") %>%
      vlbuildr::vl_encode_y(target_var, "quantitative") %>%
      vlbuildr::vl_mark_bar()

    target_line_chart <- vlbuildr::vl_chart() %>%
      vlbuildr::vl_add_data(values = factor_results$analysis_table[[1]] %>%
                    mutate(Label_delta = target_line)) %>%
      vlbuildr::vl_encode_y("Label_delta", "quantitative") %>%
      vlbuildr::vl_mark_rule(size = 2)

    return(vl_layer(barchart, target_line_chart))
  } else if (type == 'count_all') {
    barchart <- vlbuildr::vl_chart() %>%
      vlbuildr::vl_add_data(values = analysis_result) %>%
      vlbuildr::vl_encode_x(variable_name, "nominal") %>%
      vlbuildr::vl_encode_y("count", "quantitative") %>%
      vlbuildr::vl_mark_bar(color = 'salmon')
    return(barchart)
  }
}

plot_all_analysis_results_vl <- function(analysis_results) {
  avg_lst <- map(analysis_results$analysis_table, plot_analysis_category_vl)
  count_lst <- map(analysis_results$analysis_table, plot_analysis_category_vl, type = 'count_all')

  lst <- append(avg_lst, count_lst)
  lst[['columns']] <- length(avg_lst)
  do.call(vl_concat, lst)
}

