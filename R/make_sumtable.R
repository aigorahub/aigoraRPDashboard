
#' Sumtable
#'
#' @param data input dataframe
#' @param .products_colname products column
#' @param .attributes_colname attribute names column
#' @param .attribute_value_colname attribute values column
#' @param .id_cols id columns
#' @param selected_products vector of product names to be selected
#' @param selected_attributes vector of attributes names to be selected
#'
#' @return c("tbl_df", "tbl", "data.frame")
#' @export
#'
#' @examples
#' data <- mtcars
#' data$product <- rownames(data)
#'
#' data <- data %>%
#'   tidyr::pivot_longer(cols = -c(product, cyl))
#'
#' data_transformed <- make_sumtable(data = data,
#'                                   .products_colname = product,
#'                                   .attributes_colname = name,
#'                                   .attribute_value_colname = value,
#'                                   .id_cols = -value,
#'                                   selected_products = c("Merc 280", "Fiat 128", "Mazda RX4"),
#'                                   selected_attributes = c("mpg", "disp"))
#'
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
