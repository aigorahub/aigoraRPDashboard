make_panel_plot_list <-
  function(tidy_data,
           # sg_list,
           # ag_list,
           # use_cluster_order_sg,
           use_cluster_order_ag,
           split_by_ag,
           ...) {

    # sg_table <- sg_list %>%
    #   table_from_list(name_string = "Sample Group", value_string = "Sample_Name") %>%
    #   mutate_all(factor)
    # browser()

    # ag_table <- ag_list %>%
    #   table_from_list(name_string = "Attribute Group", value_string = "Attribute") %>%
    #   mutate_all(factor)

    # prepare for plotting for product inspection ----
    # NOTE: filter out any samples or attributes not in the sample or attribute tables
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

    panel_data <- tidy_data %>%
      # filter(Product %in% sg_table$Sample_Name) %>%
      # filter(Attribute %in% ag_table$Attribute) %>%
      droplevels() %>%
      mutate(
        Product = factor(Product_Name),
        Panelist = factor(Panelist_Name),
        Attribute = factor(Attribute_Name)
      ) %>%
      select(Panelist, Product, Attribute, Response = Attribute_Value)

    # create panelist by product heatmap ----

    panelist_by_prod_data <- panel_data %>%
      group_by(Panelist, Product) %>%
      summarise(mean = mean(Response)) %>%
      ungroup()


    # keep original order

    ordered_samples <- panelist_by_prod_data %>%
      pull(Product) %>%
      levels()

    ordered_panelists <- panelist_by_prod_data %>%
      pull(Panelist) %>%
      levels()



    panelist_by_prod_mid_mean <- panelist_by_prod_data %>%
      pull(mean) %>%
      range() %>%
      mean()

    panelist_by_prod_plot_data <- panelist_by_prod_data %>%
      # inner_join(sg_table, by = c("Product" = "Sample_Name")) %>%
      mutate(Product = factor(Product, levels = ordered_samples)) %>%
      mutate(Panelist = factor(Panelist, levels = ordered_panelists))

    panelist_by_prod_heatmap <- panelist_by_prod_plot_data %>%
      ggplot(aes(
        x = Panelist,
        y = fct_rev(Product),
        fill = mean
      )) +
      geom_tile() +
      scale_x_discrete(guide = guide_axis(n.dodge = 3), position = "top") +
      scale_fill_gradient(
        name = "Mean Rating",
        #labels = scales::percent_format(accuracy = 1),
        low = white_b,
        high = orange_b
      ) +
      theme(
        axis.line = element_blank(),
        panel.background = element_blank(),
        # plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
        # panel.grid = element_blank(),
        # legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
        # legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
        # legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
        # text = element_text(
        #   size = font_size,
        #   family = font_name,
        #   color = body_text_col,
        # ),
        legend.position = "bottom",

        axis.title = element_blank()
      ) +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

    # if (split_by_sg) {
    #   panelist_by_prod_heatmap <- panelist_by_prod_heatmap +
    #     facet_wrap(~ `Sample Group`, ncol = 1, scales = "free_y")
    #
    # }

    # create panelist by attribute heatmap ----

    panelist_by_attrib_data <- panel_data %>%
      group_by(Panelist, Attribute) %>%
      summarise(mean = mean(Response)) %>%
      ungroup()

    if (use_cluster_order_ag) {
      only_mean_mat <- panelist_by_attrib_data %>%
        pivot_wider(
          names_from = Panelist,
          values_from = mean
          #values_fn = mean
        ) %>%
        column_to_rownames(var = "Attribute") %>%
        as.matrix()

      # order samples

      res_hclust <- only_mean_mat %>%
        dist() %>%
        hclust(method = "ward.D2")

      order_hclust <- res_hclust %>%
        order.hclust()

      ordered_attributes <- rownames(only_mean_mat)[order_hclust]

      # order panelists

      res_hclust <- only_mean_mat %>%
        t() %>%
        dist() %>%
        hclust(method = "ward.D2")

      order_hclust <- res_hclust %>%
        order.hclust()

      ordered_panelists <-
        rownames(only_mean_mat %>% t())[order_hclust]

    } else {

      # keep original order

      sort_by_regex <- function(x, regex) {
        out <- c()
        for(r in regex) {
          out <- c(out, x[grepl(r, x)])
          x <- setdiff(x, out)
        }
        c(out, x)
      }

      ordered_attributes <- panelist_by_attrib_data %>%
        pull(Attribute) %>%
        levels()
      # sort_by_regex(c("^B_", "^FP_", "^D_", "^A_"))

      ordered_panelists <- panelist_by_attrib_data %>%
        pull(Panelist) %>%
        levels()

    }

    panelist_by_attrib_mid_mean <- panelist_by_attrib_data %>%
      pull(mean) %>%
      range() %>%
      mean()

    panelist_by_attrib_plot_data <- panelist_by_attrib_data %>%
      # inner_join(ag_table) %>%
      mutate(Attribute = factor(Attribute, levels = ordered_attributes)) %>%
      mutate(Panelist = factor(Panelist, levels = ordered_panelists))

    panelist_by_attrib_heatmap <- panelist_by_attrib_plot_data %>%
      ggplot(aes(
        x = Panelist,
        y = fct_rev(Attribute),
        fill = mean
      )) +
      geom_tile() +
      scale_x_discrete(guide = guide_axis(n.dodge = 3), position = "top") +
      scale_fill_gradient(
        name = "Mean Ratings",
        #labels = scales::percent_format(accuracy = 1),
        low = white_b,
        high = orange_b
      ) +
      theme(
        axis.line = element_blank(),
        panel.background = element_blank(),
        # plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
        # panel.grid = element_blank(),
        # legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
        # legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
        # legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
        # text = element_text(
        #   size = font_size,
        #   family = font_name,
        #   color = body_text_col,
        # ),
        legend.position = "bottom",

        axis.title = element_blank()
      ) +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))


    # create list of plots for this module ----

    inspect_panel_plot_list <-
      list("Sample by Panelist" = panelist_by_prod_heatmap,
           "Attribute by Panelist" = panelist_by_attrib_heatmap) %>%
      list("Panel Diagnostics" = .)

    return(inspect_panel_plot_list)

  }
