data <- openxlsx::read.xlsx(file.path("tests", "testdata", "Study1.xlsx"))%>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
                      names_to = "Attribute_Name",
                      values_to = "Attribute_Value") %>%
  dplyr::mutate(Study = "Study1")
cata_summary_table <- make_cata_tables(data, Product_Name, Attribute_Name, Attribute_Value)
plot(cata_summary_table)
