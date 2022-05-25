getAllsData <-function(panel_data,header_vars){

    panel_data <- panel_data %>%
      as_tibble() %>%
      mutate(product = paste(product, rep, sep = " - rep")) %>%
      select(-rep)



  header_vars <- c("product", "panelist")

  data_ind_panelist <- panel_data %>%
    pivot_longer(-all_of(header_vars), names_to = "attribute", values_to = "value")

  # group_by(attribute) %>%
  # nest() %>%
  # ungroup()
  # browser()
  data_panel<- panel_data %>%
    group_by(product) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(-product, names_to = "attribute", values_to = "panel") %>%

    mutate(panelist = "panel") %>%
    select(product, panelist, everything()) %>%
    rename(value = panel)
  # group_by(attribute) %>%
  # nest() %>%
  # rename(panel = data)

  data_list <- data_panel %>%
    bind_rows(data_ind_panelist) %>%
    group_by(attribute) %>%
    nest()



  return(data_list)

}
#' Make plot list by panelist & Product (All Panelists)
#'
#' @param per_attribute
#' @param attribute
#' @param panelist_plot
#' @param
#'
#' @return
#' @export
#'
#' @examples
plot_all_panelist_on_attr <- function(per_attribute,
                                      attribute,
                                      panelist_plot,
                                      scale_lim =-1) {
  # browser()

  # palette <- c("black", ggthemes::tableau_color_pal("Classic 20")(20))
  palette <- c("black",  colorRampPalette(brewer.pal (12, "Dark2")) (50)) # Expand the number of colors
  linesize <- c(1.3, rep(0.5, 50)) ###! change from 20 to 50

  plot_data <- per_attribute %>%
    filter (panelist %in% c("panel", panelist_plot)) %>%
    # filter(panelist != "panel") %>%
    mutate(panelist = panelist %>%
             factor() %>%
             fct_relevel("panel")) %>%
    ggplot(aes(x = product, y = value, group = panelist, color = panelist, size = panelist)) +
    geom_line() +
    # ggthemes::scale_color_tableau("Classic 20") +
    # geom_line(data = filter(per_attribute, panelist == "panel"), size = 1.3, color = "black") +
    scale_color_manual(values = palette) +
    scale_size_manual(values = linesize) +
    ggtitle(paste0('Attribute: ', attribute)) +
    # scale_color_brewer(palette = "Accent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(10, 10, 10, 100)) + # extra room for x-axix labels) +
    {if(scale_lim != -1){

      # expand_limits(y = c(0, scale_lim)) # Define scale_y
      scale_y_continuous(limits = c(0,scale_lim), breaks = seq(0, scale_lim, by = 25)) # !fix breaks
    }}  +
    labs(x = "Product", y = "Value") +
    scale_x_discrete(expand = c(0, 0), waiver()) # REmove extra space before and after plot space


}

library(RColorBrewer)

panel_data <- openxlsx::read.xlsx("tests/testdata/Study1.xlsx")%>%
  # tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
  #                     names_to = "Attribute_Name",
  #                     values_to = "Attribute_Value") %>%
  dplyr::mutate(Study = "Study1") %>%
  rename(rep = Study, product = Product_Name, panelist = Panelist_Name) %>%
  mutate(rep = case_when(panelist %in% c("Panelist6", "Panelist7", "Panelist8", "Panelist9", "Panelist10") ~ 2,
                         TRUE ~ 1),
         panelist = case_when(panelist == "Panelist6" ~ "Panelist1",
                              panelist == "Panelist7" ~ "Panelist2",
                              panelist == "Panelist8" ~ "Panelist3",
                              panelist == "Panelist9" ~ "Panelist4",
                              panelist == "Panelist10" ~ "Panelist5",
                              TRUE ~ panelist)) %>%
  select(product, panelist, rep, everything())

header_vars <- c("product", "panelist")



# Plot Function ---------------

panelist_plot <- panel_data %>% pull(panelist) %>% unique()
configVls$panelistIDall <-configVls$panelist # Dashboard option
header_vars <- c("product", "panelist")

d<-getAllsData(panel_data, header_vars)
scaleLimitAll = 150
plot_all_p <- d %>%
  mutate(plot = pmap(.f = plot_all_panelist_on_attr,
                     .l = list(data, attribute),
                     panelist_plot = panelist_plot,

                     scale_lim = scaleLimitAll)
  )
