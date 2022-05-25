# Outlier Definition

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


# Make boxplots
makeBoxPlots<-function(data, attributes, fillColor="gray", scale_lim = -1){
  # browser()
  data %>%
    group_by(product, panelist) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    pivot_longer(all_of(attributes)) %>%
    group_by(product, name) %>%
    mutate(outlier_label=if_else(is_outlier(value), panelist, as_factor(NA))) %>%
    split(.$name) %>%
    imap(~{
      ggplot(.x, aes(product, value)) +
        geom_boxplot(fill = fillColor) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Product", y =  Hmisc::capitalize(.y)) +
        {if(scale_lim != -1){

          expand_limits(y = c(0, scale_lim))  # Define scale_y
        }}  +
        geom_text_repel(aes(label=outlier_label),na.rm=TRUE,nudge_y=0.05, hjust=-0.3, size = 3, direction = "y")
    }) %>%
    set_names(paste(Hmisc::capitalize(names(.)), "Boxplot"))

}

library(Hmisc)
library(ggrepel)
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
  select(product, panelist, rep, everything()) %>%
  mutate(across(where(is.character),as_factor))

attributes <- panel_data %>%
  select(- c(product, panelist, rep))

boxPlots<-makeBoxPlots(panel_data, names(attributes), "#025C3B", 15)
