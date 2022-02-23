make_sumtable <- function(data,
                          .products_colname,
                          .attributes_colname,
                          .attribute_value_colname,
                          .id_cols,
                          selected_products,
                          selected_attributes){
  # browser()
  data %>%
    dplyr::filter({{.products_colname}} %in% unique(selected_products),
                  {{.attributes_colname}} %in% unique(selected_attributes)) %>%
    dplyr::group_by(dplyr::across({{.id_cols}})) %>%
    dplyr::summarize(Mean_scores = round(mean({{.attribute_value_colname}}), 2)) %>%
    dplyr::ungroup()
}
