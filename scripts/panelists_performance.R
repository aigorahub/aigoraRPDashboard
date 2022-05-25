#'Make Discrimination Table
#'
#' @param data
#' @param
#'
#' @return
#' @export
#'
#' @examples
DiscriminationTable <-function(data){

  # Define range
  bg_picker <- scales::col_bin(palette = c("#86c490", "#C48886"),
                               domain = c(0,1),
                               bins = c(0,0.05,1))

  data$prob.ind %>%
    as.data.frame() %>%
    rownames_to_column(var = "Panelist") %>%
    as_tibble() %>%
    # set_names(str_replace(colnames(.), "_", " ")) %>%
    # set_names(str_to_title(colnames(.))) %>%
    # select(-Median) %>%
    # filter(Panelist!="median") %>%
    flextable() %>%
    colformat_double(j = -1, digits = 3) %>%
    bg(j = -1, bg = bg_picker) %>%
    set_caption("Panelist Discrimination") %>%
    align(align = "center") %>%
    bold(bold = FALSE, part = "header") %>%
    fontsize(size = 10, part = "all")
}

#'Make repeatability Table
#'
#' @param data
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
RepeatabilityTable <-function(data, threshold = 10){

  # Define range
  bg_picker <- scales::col_bin(palette = c("#86c490", "#C48886"),
                               domain = c(0,150),
                               bins = c(0,threshold,150))

  # std <1 : Good / Std> 1 : Not Good (Using normal scale of 15 cm)
  # In this case (range: 0 - 150) --> #std <15 : Good / Std >15 : Not Good

  data$res.ind %>%
    as.data.frame() %>%
    rownames_to_column(var = "Panelist") %>%
    as_tibble() %>%
    # set_names(str_replace(colnames(.), "_", " ")) %>%
    # set_names(str_to_title(colnames(.))) %>%
    #  select(-Median) %>%
    #  filter(Panelist!="median") %>%
    flextable() %>%
    colformat_double(j = -1, digits = 3) %>%
    theme_vanilla() %>%
    bg(j = -1, bg = bg_picker) %>%
    set_caption("Panelist Repeatability") %>%
    align(align = "center") %>%
    bold(bold = FALSE, part = "header") %>%
    fontsize(size = 10, part = "all")
}

#'Make Agreement Table
#'
#' @param data
#' @param
#'
#' @return
#' @export
#'
#' @examples
AgreementTable <-function(data, threshold = 0.80){

  # Define range
  bg_picker <- scales::col_bin(palette = c("#C48886", "#86c490"),
                               domain = c(-1,+1),
                               bins = c(-1,threshold,+1))

  data$agree.ind %>%
    as.data.frame() %>%
    rownames_to_column(var = "Panelist") %>%
    as_tibble() %>%
    # set_names(str_replace(colnames(.), "_", " ")) %>%
    # set_names(str_to_title(colnames(.))) %>%
    # select(-Median) %>%
    # filter(Panelist!="median") %>%
    flextable() %>%
    colformat_double(j = -1, digits = 3) %>%
    theme_vanilla() %>%
    bg(j = -1, bg = bg_picker)  %>%
    set_caption("Agreement between panelists") %>%
    align(align = "center") %>%
    bold(bold = FALSE, part = "header") %>%
    fontsize(size = 10, part = "all")
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
                              TRUE ~ panelist)) %>%
  select(product, panelist, rep, everything())

res.paneliperf <- paneliperf(panel_data, formul = "~product+panelist+rep+product:panelist+product:rep+panelist:rep", formul.j = "~product+rep", col.j = 2, firstvar = 4, synthesis = TRUE)

# 1) prob.ind - p-value of product (discrimination)


res_dis_flx <- DiscriminationTable(data = res.paneliperf)



#2) res.ind - residuals related to ANOVa - repeatibility


res_rep_flx_tbl <- RepeatabilityTable(data = res.paneliperf)


# 3) agree.ind - agreement between panelist and whole team


res_agre_flx_tbl <- AgreementTable(data = res.paneliperf)


