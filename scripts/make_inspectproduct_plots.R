# function to make plot list for inspect products section

prep_ag_table <- function(ag_info,
                        .Attribute_Name_colname,
                        .Attribute_Group_colname){
  ag_table <- ag_info %>%
    rename(`Attribute Group` = {{.Attribute_Group_colname}}, Attribute = {{.Attribute_Name_colname}}) %>%
    mutate_all(factor)
}


prep_inspect_product_data <- function(tidy_data,
                                      .Attribute_Name_colname,
                                      .Product_Name_colname,
                                      .Attribute_Value_colname){
  cluster_data <- tidy_data %>%
    group_by({{.Product_Name_colname}}, {{.Attribute_Name_colname}}) %>%
    summarise(mean = mean({{.Attribute_Value_colname}})) %>%
    ungroup()

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
    pivot_wider(
      names_from = {{.Attribute_Name_colname}},
      values_from = mean
    ) %>%
    column_to_rownames(var = rlang::as_label(rlang::enquo(.Product_Name_colname))) %>%
    as.matrix()

  # order samples

  res_hclust <- cluter_mat %>%
    dist() %>%
    hclust(method = "ward.D2")

  order_hclust <- res_hclust %>%
    dendextend::order.hclust()

  ordered_samples <- rownames(cluter_mat)[order_hclust]

  # order attributes

  res_hclust <- cluter_mat %>%
    t() %>%
    dist() %>%
    hclust(method = "ward.D2")

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
    rename(Attribute = {{.Attribute_Name_colname}},
           Product = {{.Product_Name_colname}}) %>%
    inner_join(ag_table) %>%
    # inner_join(sg_table, by = c("Product" = "Sample_Name")) %>%
    mutate(Product = factor(Product)) %>%
    mutate(Attribute = factor(Attribute))
}

make_heatmap_product <- function(heatmap_plot_data,
                                 split_by_ag = FALSE){
  heatmap_plot <- heatmap_plot_data %>%
    ggplot(aes(
      x = Attribute,
      y = fct_rev(Product),
      fill = mean
    )) +
    geom_tile() +
    scale_fill_gradient(
      name = "Mean Rating",
      low = "#FFFFFF",
      high = "#ef7d00"
    ) +
    theme(
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),

      legend.position = "bottom",
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    coord_flip()

  if (split_by_ag) {
    heatmap_plot <- heatmap_plot +
      facet_wrap(~ `Attribute Group`, nrow = 1, scales = "free_x")

  }

  return(heatmap_plot)
}

make_corr_plot <- function(tidy_data,
                           .Attribute_Name_colname,
                           .Product_Name_colname){
  tidy_data %>%
    rename(Attribute = {{.Attribute_Name_colname}},
           Product = {{.Product_Name_colname}}) %>%
    pivot_wider(
      id_cols = c(Product),
      names_from = "Attribute",
      values_from = Attribute_Value,
      values_fn = mean
    ) %>%
    select(-Product) %>%
    cor() %>%
    ggcorrplot::ggcorrplot(
      method = "square",
      type = "lower",
      hc.method = "ward.D2",
      outline.color = "#004f9f",
      legend.title = "Correlation"
    ) +
    scale_fill_gradient2(name = "Correlation", low = "#FFFFFF", mid = "#ffbb00", high = "#ef7d00", midpoint = 0)
}

prep_barplot_data <- function(cluster_data,
                              ag_table,
                              .Attribute_Name_colname,
                              slide_var){
  cluster_data %>%
    rename(Attribute = {{.Attribute_Name_colname}}) %>%
    inner_join(ag_table) %>%
    split(.[[slide_var]])
}

prep_spiderplot_data <- function(cluster_data,
                                 ag_table,
                                 .Attribute_Name_colname){
  cluster_data %>%
    rename(Attribute = {{.Attribute_Name_colname}}) %>%
    inner_join(ag_table) %>%
    group_by(`Attribute Group`) %>%
    mutate(attribute_elim = length(unique(Attribute))) %>%
    filter(attribute_elim > 1) %>%
    select(-attribute_elim) %>%
    ungroup() %>%
    split(.$`Attribute Group`) %>%
    map(droplevels)
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
      select(all_of(bar_var), all_of(fill_var), mean) %>%
      mutate(label = '') %>%
      set_names(c("bar_var", "fill_var", "mean", "label"))

    bar_var_match <- all(plot_data$bar_var %in% ag_table$Attribute)
    fill_var_match <- all(plot_data$fill_var %in% ag_table$Attribute)

    if(bar_var_match){
      plot_data <- plot_data %>%
        mutate(bar_var = factor(bar_var, levels = ag_table$Attribute))
    }
    if(fill_var_match){
      plot_data <- plot_data %>%
        mutate(fill_var = factor(fill_var, levels = ag_table$Attribute))
    }
    colourCount = length(unique(plot_data$fill_var))
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))

    output <- plot_data %>%
      mutate(bar_var=as.factor(split_text_to_lines(as.character(bar_var), max_char_in_line = 7))) %>%
      ggplot(aes(x = bar_var, y = mean, fill = fill_var)) +
      geom_col(position = "dodge") +

      scale_y_continuous(breaks = c(0:scale_lim), limits = c(0, scale_lim)) +
      ylab("Mean Score") +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),

        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#cfcbca"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        plot.title = element_text(hjust = 0.5)

      ) +
      geom_text(aes(label = label),
                position = position_dodge(w = 0.9),
                vjust = -0.5
      ) +
      scale_fill_manual(values = getPalette(colourCount))

    return(output)
  }

split_text_to_lines <- function(text, max_char_in_line = 10){
  letters <- strsplit(text, "")[[1]]
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
}
split_text_to_lines <- Vectorize(split_text_to_lines)


make_radar_plot <- function(radar_plot_data
) {
  # browser()
  radar_plot_data %>%
    select(Product_Name, Attribute, mean) %>%
    pivot_wider(names_from = Attribute, values_from = mean) %>%
    column_to_rownames("Product_Name") %>%
    mutate(across(where(is.numeric), ~(. - min(.))/(max(.) - min(.)))) %>%
    mutate(across(where(is.numeric), replace_na, 0.5)) %>%
    mutate(group = rownames(.)) %>%
    relocate(group, .before = everything()) %>%
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
    guides(color = guide_legend(nrow = 2)) +
    coord_equal(clip = "off")
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
    map(
      make_bar_plot,
      bar_var = bar_var,
      fill_var = fill_var,
      # split_var = split_var,
      scale_lim = scale_lim,
      ag_table = ag_table
    ) %>%
    list("Bar Plots by Attribute" = .)


  spider_plot_list <- spiderplot_data %>%
    map(
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

ag_info <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx", sheet = 2)
tidy_data <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx")%>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
                      names_to = "Attribute_Name",
                      values_to = "Attribute_Value") %>%
  dplyr::mutate(Study = "Study1")

cluster_data <- prep_inspect_product_data(tidy_data = tidy_data,
                                          .Attribute_Name_colname = Attribute_Name,
                                          .Product_Name_colname =  Product_Name,
                                          .Attribute_Value_colname =  Attribute_Value)

plot(cluster_data = cluster_data,
     tidy_data = tidy_data,
     ag_info = ag_info,
     .Attribute_Name_colname = Attribute_Name,
     .Product_Name_colname =  Product_Name,
     .Attribute_Group_colname = Group,
     split_by_ag = FALSE)
