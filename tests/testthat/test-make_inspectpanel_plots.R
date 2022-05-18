# data <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx")%>%
#   tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
#                       names_to = "Attribute_Name",
#                       values_to = "Attribute_Value") %>%
#   dplyr::mutate(Study = "Study1")
# panel_data <- prep_panel_data(data, Attribute_Name, Product_Name, Panelist_Name, Attribute_Value)
# plot(panel_data)
