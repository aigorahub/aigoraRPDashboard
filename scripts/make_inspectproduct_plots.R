# function to make plot list for inspect products section

make_products_plot_list <- function(tidy_data,
                                    ag_info,
                                    use_cluster_order,
                                    split_by_ag,
                                    ag_list,
                                    ...) {
  # browser()

  alpha_level = 0.05

  ag_table <- ag_info %>%
    rename(`Attribute Group` = Attribute_Group, Attribute = Attribute_Name) %>%
    mutate_all(factor)

  # prepare for plotting for product inspection ----


  header_vars <- c("Study", "Product_Name", "Panelist_Name")

  data2 <- tidy_data %>%
    pivot_wider(id_cols = -c(Attribute_Group, Attribute_Type), names_from = Attribute_Name, values_from = Attribute_Value) %>%
    select(-Date)

  data3 <- data2 %>%
    select(-all_of(header_vars)) %>%
    select(where(~ sum(.) != 0)) %>%
    bind_cols(data2 %>% select(all_of(header_vars))) %>%
    select(all_of(header_vars), everything()) %>%
    as.data.frame()

  res_decat <- SensoMineR::decat(data3, firstvar=5, formul = ~ Product_Name + Panelist_Name + Study,
                                 proba = alpha_level, graph=FALSE)

  sign_attr <- res_decat$resF %>%
    rownames_to_column("Attribute") %>%
    pull(Attribute)

  aov_data <- data3 %>%
    pivot_longer(-c(Study:Panelist_Name), names_to = "Attribute", values_to = "Response") %>%
    rename(Sample_Name = Product_Name, Unique_Panelist_ID = Panelist_Name, Session_Name = Study) %>%
    mutate(Sample_Name = str_replace(Sample_Name, "&", "_")) %>%
    mutate(Sample_Name = as.factor(Sample_Name)) %>%
    split(.$Attribute) %>%
    `[`(sign_attr) %>%
    map(select, -Attribute) %>%
    map(aov_by_attr)

  lsd_groups <- aov_data %>%
    map(lsd_by_attr, alpha_level) %>%
    map(rownames_to_column, "Sample_Name") %>%
    enframe(name = "Attribute") %>%
    unnest(cols = value)


  # compute mean for all plotting in this section
  cluster_data <- tidy_data %>%
    group_by(Product_Name, Attribute_Name) %>%
    summarise(mean = mean(Attribute_Value)) %>%
    ungroup()

  mean_tbl <- cluster_data %>%
    pivot_wider(
      names_from = Attribute_Name,
      values_from = mean
    ) %>%
    relocate(any_of(ag_info$Attribute_Name), .after = everything())


  # create complex heatmap for product data (just one plot total but can be sorted or faceted) ----

  if (use_cluster_order) {
    cluter_mat <- cluster_data %>%
      pivot_wider(
        names_from = Attribute,
        values_from = mean
      ) %>%
      column_to_rownames(var = "Product") %>%
      as.matrix()

    # order samples

    res_hclust <- cluter_mat %>%
      dist() %>%
      hclust(method = "ward.D2")

    order_hclust <- res_hclust %>%
      order.hclust()

    ordered_samples <- rownames(cluter_mat)[order_hclust]

    # order attributes

    res_hclust <- cluter_mat %>%
      t() %>%
      dist() %>%
      hclust(method = "ward.D2")

    order_hclust <- res_hclust %>%
      order.hclust()

    ordered_attribs <-
      rownames(cluter_mat %>% t())[order_hclust]


  } else {
    # keep original order

    ordered_samples <- cluster_data %>%
      pull(Product_Name) %>%
      levels()

    ordered_attribs <- cluster_data %>%
      pull(Attribute_Name) %>%
      levels()
  }

  heatmap_plot_data <- cluster_data %>%
    rename(Attribute = Attribute_Name) %>%
    inner_join(ag_table) %>%
    # inner_join(sg_table, by = c("Product" = "Sample_Name")) %>%
    mutate(Product = factor(Product_Name)) %>%
    mutate(Attribute = factor(Attribute))

  mid_mean <- heatmap_plot_data %>%
    pull(mean) %>%
    range() %>%
    mean()

  # Bartek

  dark_blue_b <- "#0e2b63"
  mid_blue_b <- "#004f9f"
  cyan_b <- "#00b1eb"
  sky_b <- "#0c61a7"
  orange_b <- "#ef7d00"
  green_b <- "#50af47"
  purple_b <- "#5a328a"
  yellow_b <- "#ffbb00"
  light_green_b <- "#afca0b"
  magenta_b <- "#e72582"
  grey_b <- "#575756"
  red_b <- "#eb4a00"
  brown_b <- "#632900"
  yellow_gray_b <- "#cfcc2a"
  rose_b <- "#af474f"
  dirt_b <- "#8a5c32"
  blue_green_b <- "#328a83"
  blue_purple_b <- "#4756af"
  light_brown_b <- "#af7747"
  purple_red_b <- "#a647af"


  white_b <- "#FFFFFF"

  heatmap_plot <- heatmap_plot_data %>%
    ggplot(aes(
      x = Attribute,
      y = fct_rev(Product),
      fill = mean
    )) +
    geom_tile() +
    scale_fill_gradient(
      name = "Mean Rating",
      low = white_b,
      high = orange_b
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



  # create correlogram (just one plot total, no sorting or faceting) ----

  corr_plot <- tidy_data %>%
    rename(Attribute = Attribute_Name) %>%
    semi_join(ag_table) %>%

    pivot_wider(
      id_cols = -c(Attribute_Type, Date, Attribute_Group),
      names_from = "Attribute",
      values_from = Attribute_Value,
      values_fn = mean
    ) %>%
    select(-Study, -Panelist_Name, -Product_Name) %>%
    cor() %>%
    ggcorrplot(
      method = "square",
      type = "lower",
      hc.method = "ward.D2",
      outline.color = mid_blue_b,
      legend.title = "Correlation"
    ) +
    scale_fill_gradient2(name = "Correlation", low = white_b, mid = yellow_b, high = orange_b, midpoint = 0)

  # Combine heatmap table and correlation plot into a single section ----

  inspect_products_plot_list <-
    list("Products by Attributes" = heatmap_plot,
         "Attribute Correlation" = corr_plot) %>%
    list("Product Diagnostics" = .)



  # Make bar plots by attribute
  slide_var <- "Attribute Group"

  as_bar_plot_data_list <- cluster_data %>%
    rename(Attribute = Attribute_Name) %>%
    inner_join(ag_table) %>%
    split(.[[slide_var]])

  bar_var <- "Product_Name"
  fill_var <- "Attribute"
  scale_lim <- 7



  as_bar_plot_list <- as_bar_plot_data_list %>%
    map(
      make_bar_plot,
      bar_var = bar_var,
      fill_var = fill_var,
      # split_var = split_var,
      scale_lim = scale_lim,
      ag_table = ag_table
    ) %>%
    list("Bar Plots by Attribute" = .)

  # Make bar plots by sample

  # make spider plots - one plot per pair in sg_list x ag_list ----

  spider_plot_data_list <- cluster_data %>%
    rename(Attribute = Attribute_Name) %>%
    inner_join(ag_table) %>%
    group_by(`Attribute Group`) %>%
    mutate(attribute_elim = length(unique(Attribute))) %>%
    filter(attribute_elim > 1) %>%
    select(-attribute_elim) %>%
    ungroup() %>%
    split(.$`Attribute Group`) %>%
    map(droplevels)

  spider_plot_list <- spider_plot_data_list %>%
    map(
      .f = make_radar_plot
    )

  as_bar_plot_list = as_bar_plot_list[[1]]
  #added by hamza


  as<-list()

  for(p in 1:length(as_bar_plot_list)){
    as[[paste("Samples Mean Score", p)]]<-as_bar_plot_list[[p]]
  }

  spider_plot_list<-list("Radar Plots by Attributes"  =spider_plot_list )
  as_bar_plot_list<-list("Bar Plots by Attributes" =as )
  product_plot_list <-
    c(
      inspect_products_plot_list,
      as_bar_plot_list,
      spider_plot_list
    )


  return(list(product_plot_list, mean_tbl))
}

# functions for aov and lsd analysis

aov_by_attr <- function (aov_data){

  aov(Response ~ Sample_Name + Unique_Panelist_ID + Session_Name, data=aov_data)
}

lsd_by_attr <- function(model, alpha_level) {

  lsd_res <- agricolae::LSD.test(model, "Sample_Name", p.adj = "none", group = TRUE, alpha = alpha_level)

  return(lsd_res$groups)
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
