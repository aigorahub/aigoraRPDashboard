# data <- openxlsx::read.xlsx("tests/testdata/Study1.xlsx")
#
# attributes <- data %>%
#   select(- c(Product_Name, Panelist_Name)) %>%
#   names()
#
# spider_data <- data %>%
#   pivot_longer(attributes, names_to="Variables", values_to="Scores") %>%
#   mutate_at ("Variables", str_replace, "_", " ") %>%
#   mutate(Variables = fct_inorder(Variables)) %>%
#   rename(Product = Product_Name) %>%
#   group_by(Product, Variables) %>%
#   summarize(Mean = mean(Scores)) %>%
#   ungroup()
#
#
# font_name <- "Aeroport"
# font_size <- 9
# background_fill <- "#FFFFFF"
# body_bg_col <- "white"
#
# spider_PLOT<- spider_data %>%
#   make_radar_plot(
#     font_name = font_name,
#     font_size = font_size,
#     background_fill = body_bg_col,
#     values.radar = c(0, 5, 10))
#
# spider_PLOT<- list(spider_PLOT, 25, 50, 100, 125) %>%
#   reduce(add_grid_to_ggradar)
