#' @export
makePairwisePlot<-function(attr1,
                           attr2,
                           table,
                           body_bg_col = "white",
                           body_text_col = "black",
                           font_size = 12
){
  attr_pair <- c(attr1, attr2)   #sample(dataVls$attributes, 2, replace = TRUE)
  #browser()
  attr1 <- rlang::sym(attr1)
  attr2 <- rlang::sym(attr2)

  attr_pair_mat <- stringr::str_c(attr_pair, collapse = "|")

  plotObj <- table %>%
    dplyr::select(Product_Name, Attribute_Name, Attribute_Value) %>%
    dplyr::group_by(Product_Name, Attribute_Name) %>%
    dplyr::summarize(Response = mean(Attribute_Value)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = Attribute_Name, values_from = Response) %>%
    ggplot2::ggplot(ggplot2::aes(x = !!attr1, y = !!attr2)) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(ggplot2::aes(label = Product_Name)) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      panel.grid = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      legend.key = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      legend.box.background = ggplot2::element_rect(fill = body_bg_col, color = body_bg_col),
      text = ggplot2::element_text(
        size = font_size,
        # family = font_name,
        color = body_text_col,
      ),
      legend.position = "bottom"
      #axis.title = element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))

  return(plotObj)
}
