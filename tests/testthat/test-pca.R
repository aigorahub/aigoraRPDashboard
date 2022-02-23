#
# data <- openxlsx::read.xlsx("../testdata/Study1.xlsx")%>%
#   tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
#                       names_to = "Attribute_Name",
#                       values_to = "Attribute_Value") %>%
#   dplyr::mutate(Study = "Study1")
# pca_data <- prep_pca_data(data,
#                           .id_cols = c(Product_Name, Study),
#                           .attributes_name_colname = "Attribute_Name",
#                           .attributes_value_colname = "Attribute_Value",
#                           .studies_colname = "Study",
#                           .panelists_name_colname = "Panelist_Name")
#
# plot(pca_data, num_vars_to_show = 4)

# test_that("pca works", {
#   expect_equal(2 * 2, 4)
# })
