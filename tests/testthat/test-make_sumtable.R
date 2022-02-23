library(magrittr)

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
  expect_equal(length(unique(data_transformed[["product"]])), 3)
  expect_equal(length(unique(data_transformed[["name"]])), 2)
})
