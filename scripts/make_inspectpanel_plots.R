prep_panel_data <- function(data,
                            .Attribute_Name_colname,
                            .Product_Name_colname,
                            .Panelist_Name_colname,
                            .Attribute_Value_colname){
  panel_data <- data %>%
    droplevels() %>%
    dplyr::mutate(
      Product = factor({{.Product_Name_colname}}),
      Panelist = factor({{.Panelist_Name_colname}}),
      Attribute = factor({{.Attribute_Name_colname}})
    ) %>%
    dplyr::select(Panelist, Product, Attribute, Response = {{.Attribute_Value_colname}})

  return(
    structure(
      panel_data,
      class = c("aigora_inspect_panel", "tbl_df", "tbl", "data.frame")
    ))
}

prep_plot_data <- function(){
  UseMethod("prep_plot_data")
}

prep_plot_data <- function(panel_data, .variable_to_present){

  # browser()
  panelist_by_var_data <- panel_data %>%
    dplyr::group_by(dplyr::across(c(Panelist, {{.variable_to_present}}))) %>%
    dplyr::summarise(mean = mean(Response)) %>%
    dplyr::ungroup()

  ordered_variable <- panelist_by_var_data %>%
    dplyr::pull({{.variable_to_present}}) %>%
    levels()

  ordered_panelists <- panelist_by_var_data %>%
    dplyr::pull(Panelist) %>%
    levels()

  panelist_by_var_mid_mean <- panelist_by_var_data %>%
    dplyr::pull(mean) %>%
    range() %>%
    mean()

  panelist_by_var_plot_data <- panelist_by_var_data %>%
    dplyr::mutate({{.variable_to_present}} := factor({{.variable_to_present}}, levels = ordered_variable)) %>%
    dplyr::mutate(Panelist = factor(Panelist, levels = ordered_panelists))

  return(panelist_by_var_plot_data)
}



plot_panel_heatmap <- function(prepared_panel_data,
                               .variable_to_present){

  prepared_panel_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = Panelist,
      y = forcats::fct_rev({{.variable_to_present}}),
      fill = mean
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 3), position = "top") +
    ggplot2::scale_fill_gradient(
      name = "Mean Rating",
      low = "#FFFFFF",
      high = "#ef7d00"
    ) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))

}

plot.aigora_inspect_panel <- function(panel_data){
  product_panel_data <- prep_plot_data(panel_data = panel_data, .variable_to_present = Product)
  attribute_panel_data <- prep_plot_data(panel_data = panel_data, .variable_to_present = Attribute)

  panelist_by_prod_heatmap <- plot_panel_heatmap(prepared_panel_data = product_panel_data, .variable_to_present = Product)
  panelist_by_attrib_heatmap <- plot_panel_heatmap(prepared_panel_data = attribute_panel_data, .variable_to_present = Attribute)

  inspect_panel_plot_list <-
    list("Sample by Panelist" = panelist_by_prod_heatmap,
         "Attribute by Panelist" = panelist_by_attrib_heatmap) %>%
    list("Panel Diagnostics" = .)

  return(inspect_panel_plot_list)
}

# data <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx")%>%
#   tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
#                       names_to = "Attribute_Name",
#                       values_to = "Attribute_Value") %>%
#   dplyr::mutate(Study = "Study1")
# panel_data <- prep_panel_data(data, Attribute_Name, Product_Name, Panelist_Name, Attribute_Value)
# plot(panel_data)
