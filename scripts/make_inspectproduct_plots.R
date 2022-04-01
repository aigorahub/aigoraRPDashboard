# function to make plot list for inspect products section

prep_ag_table <- function(ag_info,
                        .Attribute_Name_colname,
                        .Attribute_Group_colname){
  ag_table <- ag_info %>%
    dplyr::rename(`Attribute Group` = {{.Attribute_Group_colname}}, Attribute = {{.Attribute_Name_colname}}) %>%
    dplyr::mutate_all(factor)
}


prep_inspect_product_data <- function(tidy_data,
                                      .Attribute_Name_colname,
                                      .Product_Name_colname,
                                      .Attribute_Value_colname){
  cluster_data <- tidy_data %>%
    dplyr::group_by({{.Product_Name_colname}}, {{.Attribute_Name_colname}}) %>%
    dplyr::summarise(mean = mean({{.Attribute_Value_colname}})) %>%
    dplyr::ungroup()

  return(
    structure(
      cluster_data,
      class = c("inspect_product", "tbl_df", "tbl", "data.frame")
    )
  )
}


use_cluster_order <- function(cluster_data,
                              .Attribute_Name_colname,
                              .Product_Name_colname){
  cluter_mat <- cluster_data %>%
    tidyr::pivot_wider(
      names_from = {{.Attribute_Name_colname}},
      values_from = mean
    ) %>%
    tibble::column_to_rownames(var = rlang::as_label(rlang::enquo(.Product_Name_colname))) %>%
    as.matrix()

  # order samples

  res_hclust <- cluter_mat %>%
    factoextra::dist() %>% # TODO: make sure which package this function is called from
    stats::hclust(method = "ward.D2")

  order_hclust <- res_hclust %>%
    dendextend::order.hclust()

  ordered_samples <- rownames(cluter_mat)[order_hclust]

  # order attributes

  res_hclust <- cluter_mat %>%
    t() %>%
    factoextra::dist() %>%
    stats::hclust(method = "ward.D2")

  order_hclust <- res_hclust %>%
    dendextend::order.hclust()

  ordered_attribs <-
    rownames(cluter_mat %>% t())[order_hclust]

  return(list("ordered_products" = ordered_samples,
              "ordered_attributes" = ordered_attribs))
}

prep_heatmap_data <- function(cluster_data,
                              ag_table,
                              .Attribute_Name_colname,
                              .Product_Name_colname){
  # browser()
  cluster_data %>%
    dplyr::rename(Attribute = {{.Attribute_Name_colname}},
           Product = {{.Product_Name_colname}}) %>%
    dplyr::inner_join(ag_table) %>%
    # inner_join(sg_table, by = c("Product" = "Sample_Name")) %>%
    dplyr::mutate(Product = factor(Product)) %>%
    dplyr::mutate(Attribute = factor(Attribute))
}

make_heatmap_product <- function(heatmap_plot_data,
                                 split_by_ag = FALSE){
  heatmap_plot <- heatmap_plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = Attribute,
      y = forcats::fct_rev(Product),
      fill = mean
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      name = "Mean Rating",
      low = "#FFFFFF",
      high = "#ef7d00"
    ) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),

      legend.position = "bottom",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5)) +
    ggplot2::coord_flip()

  if (split_by_ag) {
    heatmap_plot <- heatmap_plot +
      ggplot2::facet_wrap(~ `Attribute Group`, nrow = 1, scales = "free_x")

  }

  return(heatmap_plot)
}

make_corr_plot <- function(tidy_data,
                           .Attribute_Name_colname,
                           .Product_Name_colname){
  tidy_data %>%
    dplyr::rename(Attribute = {{.Attribute_Name_colname}},
           Product = {{.Product_Name_colname}}) %>%
    tidyr::pivot_wider(
      id_cols = c(Product),
      names_from = "Attribute",
      values_from = Attribute_Value,
      values_fn = mean
    ) %>%
    dplyr::select(-Product) %>%
    stats::cor() %>%
    ggcorrplot::ggcorrplot(
      method = "square",
      type = "lower",
      hc.method = "ward.D2",
      outline.color = "#004f9f",
      legend.title = "Correlation"
    ) +
    ggplot2::scale_fill_gradient2(name = "Correlation", low = "#FFFFFF", mid = "#ffbb00", high = "#ef7d00", midpoint = 0)
}

prep_barplot_data <- function(cluster_data,
                              ag_table,
                              .Attribute_Name_colname,
                              slide_var){
  cluster_data %>%
    dplyr::rename(Attribute = {{.Attribute_Name_colname}}) %>%
    dplyr::inner_join(ag_table) %>%
    split(.[[slide_var]])
}

prep_spiderplot_data <- function(cluster_data,
                                 ag_table,
                                 .Attribute_Name_colname){
  cluster_data %>%
    dplyr::rename(Attribute = {{.Attribute_Name_colname}}) %>%
    dplyr::inner_join(ag_table) %>%
    dplyr::group_by(`Attribute Group`) %>%
    dplyr::mutate(attribute_elim = length(unique(Attribute))) %>%
    dplyr::filter(attribute_elim > 1) %>%
    dplyr::select(-attribute_elim) %>%
    dplyr::ungroup() %>%
    split(.$`Attribute Group`) %>%
    purrr::map(droplevels)
}

# create barplots (one plot per attribute, can be faceted by sample group) ----
make_bar_plot <-
  function(bar_plot_data,
           fill_var,
           bar_var,
           scale_lim,
           ag_table
  ) {
    # browser()

    plot_data <- bar_plot_data %>%
      dplyr::select(tidyselect::all_of(bar_var), tidyselect::all_of(fill_var), mean) %>%
      dplyr::mutate(label = '') %>%
      rlang::set_names(c("bar_var", "fill_var", "mean", "label"))

    bar_var_match <- all(plot_data$bar_var %in% ag_table$Attribute)
    fill_var_match <- all(plot_data$fill_var %in% ag_table$Attribute)

    if(bar_var_match){
      plot_data <- plot_data %>%
        dplyr::mutate(bar_var = factor(bar_var, levels = ag_table$Attribute))
    }
    if(fill_var_match){
      plot_data <- plot_data %>%
        dplyr::mutate(fill_var = factor(fill_var, levels = ag_table$Attribute))
    }
    colourCount = length(unique(plot_data$fill_var))
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))

    output <- plot_data %>%
      dplyr::mutate(bar_var=as.factor(split_text_to_lines(as.character(bar_var), max_char_in_line = 7))) %>%
      ggplot2::ggplot(ggplot2::aes(x = bar_var, y = mean, fill = fill_var)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_y_continuous(breaks = c(0:scale_lim), limits = c(0, scale_lim)) +
      ggplot2::ylab("Mean Score") +
      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        axis.title.x = ggplot2::element_blank(),

        legend.title = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "#cfcbca"),
        panel.background = ggplot2::element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        plot.title = ggplot2::element_text(hjust = 0.5)

      ) +
      ggplot2::geom_text(ggplot2::aes(label = label),
                position = ggplot2::position_dodge(w = 0.9),
                vjust = -0.5
      ) +
      ggplot2::scale_fill_manual(values = getPalette(colourCount))

    return(output)
  }

split_text_to_lines <- Vectorize(function(text, max_char_in_line = 10){
  letters <- stringr::str_split(text, "")[[1]]
  n_lines <- ceiling(length(letters)/max_char_in_line)

  if(length(letters) > max_char_in_line){
    for (line in 1:(n_lines-1)) {
      number_of_letter <- 1

      for (letter in rev(head(letters, line * max_char_in_line))) {
        if(letter %in% c(" ", "_")){
          letters[line * max_char_in_line - number_of_letter + 1] <- "\n"
          break
        }
        number_of_letter <- number_of_letter + 1
      }
    }

  }
  return(paste(letters, collapse = ''))
})
# split_text_to_lines <- Vectorize(split_text_to_lines)


make_radar_plot <- function(radar_plot_data
) {
  # browser()
  radar_plot_data %>%
    dplyr::select(Product_Name, Attribute, mean) %>%
    tidyr::pivot_wider(names_from = Attribute, values_from = mean) %>%
    tibble::column_to_rownames("Product_Name") %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~(. - min(.))/(max(.) - min(.)))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), replace_na, 0.5)) %>%
    dplyr::mutate(group = rownames(.)) %>%
    dplyr::relocate(group, .before = tidyselect::everything()) %>%
    as.data.frame() %>%
    ggradar::ggradar(
      group.line.width = 0.8,
      group.point.size = 2,
      axis.label.size = 3,
      legend.position = "bottom",
      label.gridline.min = FALSE,
      label.gridline.mid = FALSE,
      label.gridline.max = FALSE,
      gridline.max.linetype = 1,
      gridline.max.colour = "gray",
      gridline.mid.colour = "gray",
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::coord_equal(clip = "off")
}

plot.inspect_product <- function(cluster_data,
                                 tidy_data,
                                 ag_info,
                                 .Attribute_Name_colname,
                                 .Product_Name_colname,
                                 .Attribute_Group_colname,
                                 split_by_ag = FALSE){
  # browser()
  ag_table <- prep_ag_table(ag_info = ag_info,
                            .Attribute_Name_colname = {{.Attribute_Name_colname}},
                            .Attribute_Group_colname =  Group)

  heatmap_data <- prep_heatmap_data(cluster_data = cluster_data,
                                    ag_table = ag_table,
                                    .Attribute_Name_colname = {{.Attribute_Name_colname}},
                                    .Product_Name_colname = {{.Product_Name_colname}})

  heatmap <- make_heatmap_product(heatmap_data,
                                  split_by_ag = split_by_ag)

  corr_plot <- make_corr_plot(tidy_data,
                              .Attribute_Name_colname = {{.Attribute_Name_colname}},
                              .Product_Name_colname = {{.Product_Name_colname}})

  barplot_data <- prep_barplot_data(cluster_data = cluster_data,
                                    ag_table = ag_table,
                                    .Attribute_Name_colname = {{.Attribute_Name_colname}},
                                    slide_var = "Attribute Group")

  spiderplot_data <- prep_spiderplot_data(cluster_data = cluster_data,
                                          ag_table = ag_table,
                                          .Attribute_Name_colname = {{.Attribute_Name_colname}})


  inspect_products_plot_list <-
    list("Products by Attributes" = heatmap,
         "Attribute Correlation" = corr_plot) %>%
    list("Product Diagnostics" = .)



  # Make bar plots by attribute
  bar_var <- "Product_Name"
  fill_var <- "Attribute"
  scale_lim <- 7

  as_bar_plot_list <- barplot_data %>%
    purrr::map(
      make_bar_plot,
      bar_var = bar_var,
      fill_var = fill_var,
      # split_var = split_var,
      scale_lim = scale_lim,
      ag_table = ag_table
    ) %>%
    list("Bar Plots by Attribute" = .)


  spider_plot_list <- spiderplot_data %>%
    purrr::map(
      .f = make_radar_plot
    )

  as_bar_plot_list = as_bar_plot_list[[1]]

  as<-list()

  for(p in 1:length(as_bar_plot_list)){
    as[[paste("Samples Mean Score", p)]] <- as_bar_plot_list[[p]]
  }

  spider_plot_list<-list("Radar Plots by Attributes"  = spider_plot_list )
  as_bar_plot_list<-list("Bar Plots by Attributes" = as )
  product_plot_list <-
    c(
      inspect_products_plot_list,
      as_bar_plot_list,
      spider_plot_list
    )


  return(product_plot_list)
}
#
# ag_info <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx", sheet = 2)
# tidy_data <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx")%>%
#   tidyr::pivot_longer(cols = tidyselect::starts_with("Attribute"),
#                       names_to = "Attribute_Name",
#                       values_to = "Attribute_Value") %>%
#   dplyr::mutate(Study = "Study1")

# cluster_data <- prep_inspect_product_data(tidy_data = tidy_data,
#                                           .Attribute_Name_colname = Attribute_Name,
#                                           .Product_Name_colname =  Product_Name,
#                                           .Attribute_Value_colname =  Attribute_Value)
#
# plot(cluster_data = cluster_data,
#      tidy_data = tidy_data,
#      ag_info = ag_info,
#      .Attribute_Name_colname = Attribute_Name,
#      .Product_Name_colname =  Product_Name,
#      .Attribute_Group_colname = Group,
#      split_by_ag = FALSE)
