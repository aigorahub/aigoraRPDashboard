#' @export
make_cata_tables <- function(df_cata,
                             .Product_Name_colname,
                             .Attribute_Name_colname,
                             .Attribute_Value_colname) {
  # browser()
  cata_summary_table <- df_cata %>%
    dplyr::rename(Product = {{.Product_Name_colname}}, Attribute = {{.Attribute_Name_colname}}, Response = {{.Attribute_Value_colname}}) %>%
    dplyr::select(Product,  Attribute, Response) %>%
    dplyr::group_by(Product, Attribute) %>%
    dplyr::summarize(Total = sum(Response)) %>%
    tidyr::pivot_wider(names_from = Attribute, values_from = Total)

  return(
    structure(
      cata_summary_table,
      class = c("cata_table", "tbl_df", "tbl", "data.frame")
    )
  )
}

#' @export
plot.cata_table <- function(cata_summary_table){
  # browser()
  cata_plot <- cata_summary_table %>%
    tidyr::pivot_longer(cols= !Product, names_to = "Attributes", values_to = "Count") %>%
    split(.$Product, drop = TRUE) %>%
    purrr::imap(~{
      .x %>%
        ggplot2::ggplot(ggplot2::aes(x=Attributes, y = Count)) +
        ggplot2::geom_col() +
        ggplot2::ggtitle(unique(.x$Product)) +
        ggplot2::theme(
          legend.position="bottom",
          legend.title = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 90),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = "#cfcbca"),
          panel.background = ggplot2::element_rect(fill = "white",
                                                   colour = "white",
                                                   size = 0.5, linetype = "solid"),
          plot.title = ggplot2::element_text(hjust = 0.5))

    }
    )

  return(c(cata_plot))
}
