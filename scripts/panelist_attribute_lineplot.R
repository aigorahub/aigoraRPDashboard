getAttributesData <-function(panel_data,header_vars){
  # browser()

  panel_data <-  panel_data %>%
    as_tibble() %>%
    select(-rep)

  data_list_panelist <- panel_data %>%
    pivot_longer(-all_of(header_vars), names_to = "attribute", values_to = "value") %>%
    group_by(panelist, attribute) %>%
    nest() %>%
    ungroup() %>%
    mutate(data = map(data, find_mean_2))

  data_list_overall <- panel_data %>%
    group_by(product) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(-product, names_to = "attribute", values_to = "panel") %>%
    group_by(attribute) %>%
    nest() %>%
    rename(panel = data)

  d <- data_list_panelist %>%
    left_join(data_list_overall)

  return(d)

}

find_mean_2 <- function(x){

  x %>%
    group_by(product) %>%
    summarise(panelist = mean(value)) %>%
    ungroup()

}

#' Make plot list by Panelist & Attribute
#'
#' @param per_panel_per_att
#' @param attr_overall_mean
#' @param attribute
#' @param panelist
#'
#' @return
#' @export
#'
#' @examples
plot_panelist_vs_overall_on_attr <- function(per_panel_per_attr,
                                             attr_overall_mean,
                                             attribute,
                                             panelist,
                                             scale_lim=-1, color) { # Insert scale_lim

  plot_data <- per_panel_per_attr %>%
    left_join(attr_overall_mean) %>%
    pivot_longer(-product, names_to = "Mean Type", values_to = "value") %>%
    mutate_if(is.character, as_factor)


  plot_data %>%
    ggplot(aes(x = product, y = value, group = `Mean Type`, color = `Mean Type`)) +
    geom_line(size = 1, aes(linetype=`Mean Type`),color = color) +
    ggtitle(paste0('Attribute: ', attribute, '     ', 'Panelist: ', panelist)) +
    scale_color_brewer(palette = "Accent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(10, 10, 10, 100)) + # extra room for x-axis labels
    {if(scale_lim != -1){

      expand_limits(y = c(0, scale_lim))  # Define scale_y
    }}  +
    labs(x = "Product", y = "Value") +
    scale_x_discrete(expand = c(0, 0), waiver()) # To remove extra space before and after plot area


}

header_vars <- c("product", "panelist")

d<-getAttributesData(panel_data, header_vars)



plot_table2 <- d %>%
  # filter (panelist %in% panelist_plot, attribute %in% attribute_plot) %>%
  mutate(plot = pmap(.f = plot_panelist_vs_overall_on_attr,
                     .l = list(data, panel, attribute, panelist,
                               15, "#025C3B")) # include scale_lim argument
  )
