# ag_info <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx", sheet = 2)
# tidy_data <- openxlsx::read.xlsx("tests//testdata/Study1.xlsx")%>%
#   tidyr::pivot_longer(cols = tidyselect::starts_with("Attribute"),
#                       names_to = "Attribute_Name",
#                       values_to = "Attribute_Value") %>%
#   dplyr::mutate(Study = "Study1")
#
# cluster_data <- prep_inspect_product_data(tidy_data = tidy_data,
#                                           .Attribute_Name_colname = Attribute_Name,
#                                           .Product_Name_colname =  Product_Name,
#                                           .Attribute_Value_colname =  Attribute_Value)
#
# plot(cluster_data = cluster_data,
#      tidy_data = tidy_data,
#      ag_info = ag_info,
#      .Attribute_Name_colname = Attribute_Name,
#      .Product_Name_colname =  Product_Name,
#      .Attribute_Group_colname = Group,
#      split_by_ag = FALSE)
