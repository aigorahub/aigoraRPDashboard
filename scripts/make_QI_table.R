find_qi <- function(data, panel_data) {

  res.aov <- aov (value ~ product+panelist+rep+product:panelist+product:rep+panelist:rep+product:panelist:rep, data = data)

  tidy_res.aov <- res.aov %>%
    #summary.aov() %>%
    tidy()

  sq_product <- tidy_res.aov$sumsq[[1]] ### Need to extract only the value not a tibble 1x1
  sq_product_panelist <- tidy_res.aov$sumsq[[4]]
  sq_product_replicate <- tidy_res.aov$sumsq[[5]]
  sq_product_panelist_replicate <- tidy_res.aov$sumsq[[7]]

  n_product <- n_distinct(panel_data$product)
  n_panelist <- n_distinct(panel_data$panelist)
  n_assement <- n_product * n_panelist

  var_universe <- sq_product/n_product
  var_error <- (sq_product_panelist+sq_product_replicate+sq_product_panelist_replicate)/n_assement


  qi <- var_universe/(var_universe + var_error)

  return(qi)

}

#' Final Table
#'
#' @param decat_data
#'
#' @return flextable
#' @export
#'
#' @examples
makeQITable <-function(decat_data, qi_data){

  res.decat <- SensoMineR::decat(decat_data,
                                 formul =  "~ product+panelist+rep+product:panelist+product:rep+panelist:rep+product:panelist:rep",
                                 firstvar = 4, graph = FALSE)

  qi_tbl <- res.decat$tabF[,2] %>%
    enframe(name = "attribute", value = "p_value") %>%
    left_join(qi_data) %>%
    rename(QI = qi, pvalue = p_value, Attribute = attribute) %>%
    mutate(QI = sprintf(QI, fmt = '%#.2f')) %>%
    mutate (pvalue =sprintf(pvalue, fmt ='%#.3f'))
  # mutate_at ("Attribute", str_replace, "_", " ") %>%
  # mutate (Attribute = str_to_title(Attribute))

  qi_tbl$pvalue[qi_tbl$pvalue < 0.001] <- "< 0.001"
  qi_tbl$pvalue[qi_tbl$pvalue < 0.05  & qi_tbl$pvalue > 0.001] <- "< 0.05"

  qi_flx_tbl <- qi_tbl %>%
    flextable() %>%
    autofit() %>%
    theme_vanilla() %>%
    bg(i = ~ QI > 0.80,
       j = "QI", bg = "#86c490", part = "body") %>%
    bg(i = ~ QI < 0.50,
       j = "QI", bg = "#C48886", part = "body") %>%
    set_caption("Panel Performance") %>%
    bold(bold = FALSE, part = "header") %>%
    fontsize(size = 10, part = "all")

  return(list(qi_flx_tbl, qi_tbl))

}

library(tidyverse)
library(generics)
library(SensoMineR)
library(flextable)
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
                                TRUE ~ panelist))

header_vars <- c("product", "panelist", "rep")

panel_data_list <- panel_data %>%
  mutate(across(where(is.character),as_factor)) %>%
  pivot_longer(-all_of(header_vars), names_to = "attribute", values_to = "value") %>%
  group_by(attribute) %>%
  nest()

qi_data <- panel_data_list %>%
  mutate(qi = map_dbl(data, find_qi, panel_data)) %>%
  arrange(desc(qi)) %>%
  select(attribute, qi)

res <- makeQITable(panel_data %>%
                     as.data.frame(), qi_data)
