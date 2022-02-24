makePairwisePlot<-function(attr1,
                           attr2,
                           table,
                           body_bg_col = "white",
                           body_text_col = "black",
                           font_size = 12
                           ){
  attr_pair <- c(attr1, attr2)   #sample(dataVls$attributes, 2, replace = TRUE)
  #browser()
  attr1 <- sym(attr1)
  attr2 <- sym(attr2)

  attr_pair_mat <- str_c(attr_pair, collapse = "|")

  plotObj <- table %>%
    select(Product_Name, Attribute_Name, Attribute_Value) %>%
    group_by(Product_Name, Attribute_Name) %>%
    summarize(Response = mean(Attribute_Value)) %>%
    ungroup() %>%
    pivot_wider(names_from = Attribute_Name, values_from = Response) %>%
    ggplot(aes(x = !!attr1, y = !!attr2)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = Product_Name)) +
    theme(
      axis.line = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
      panel.grid = element_blank(),
      legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
      text = element_text(
        size = font_size,
        # family = font_name,
        color = body_text_col,
      ),
      legend.position = "bottom"
      #axis.title = element_blank()
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

  return(plotObj)
}

data <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx")%>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
                      names_to = "Attribute_Name",
                      values_to = "Attribute_Value") %>%
  dplyr::mutate(Study = "Study1")

makePairwisePlot("Attribute11_Scaled", "Attribute11_CATA", data)
