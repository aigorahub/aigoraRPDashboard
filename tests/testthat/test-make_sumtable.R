library(magrittr)
library(tidyverse)
library(openxlsx)
library(testthat)
#
#
# data <- mtcars
# data$product <- rownames(data)
#
# data <- data %>%
#   tidyr::pivot_longer(cols = -c(product, cyl)) %>%
#   rename(Prduct_Name = Product,
#          Attribute_Name)

# file_path <- file.path("tests", "testdata")
# input_filename <- "Study1.xlsx"
#
# test_data <- read.xlsx(file.path(file_path, input_filename)) %>%
#   pivot_longer(cols = -c(Product_Name, Panelist_Name, Study),
#                names_to = "Attribute_Name",
#                values_to = "Attribute_Value")
#
# data_transformed <- make_sumtable(data = test_data,
#                                   selected_products = list("Product1", "Product2", "Product3"),
#                                   selectedAttributes = list("Attribute1_Scaled", "Attribute2_Scaled", "Attribute11_Scaled", "Attribute5_Scaled"))



data <- mtcars
data$product <- rownames(data)

data <- data %>%
  tidyr::pivot_longer(cols = -c(product, cyl))

data_transformed <- make_sumtable(data = data,
                                  .products_colname = product,
                                  .attributes_colname = name,
                                  .attribute_value_colname = value,
                                  .id_cols = -value,
                                  selected_products = c("Merc 280", "Fiat 128", "Mazda RX4"),
                                  selected_attributes = c("mpg", "disp"))

test_that("sumtable returns right class", {
  expect_equal(
    class(data_transformed),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("sumtable returns correct structure", {
  expect_equal(length(unique(data_transformed$product)), 3)
  expect_equal(length(unique(data_transformed$name)), 2)
})

test_that("sumtable does not return empty table", {
  expect_equal(ncol(data_transformed) == 0, FALSE)
  expect_equal(nrow(data_transformed) != 0, TRUE)

})
