# data <- openxlsx::read.xlsx("tests/testdata/Study1.xlsx")
#
# attributes <- data %>%
#   select(- c(Product_Name, Panelist_Name)) %>%
#   names()
#
# cluster<-make_cluster_phylo(data,
#                             .id_col="Product_Name",
#                             .attributes = attributes #tidyselect::everything(),
# )
