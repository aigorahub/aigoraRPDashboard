context("make-pairwise-plots")


# data <- openxlsx::read.xlsx(file.path("tests", "testdata", "Study1.xlsx"))%>%
#   tidyr::pivot_longer(cols = dplyr::starts_with("Attribute"),
#                       names_to = "Attribute_Name",
#                       values_to = "Attribute_Value") %>%
#   dplyr::mutate(Study = "Study1")

# plot <- makePairwisePlot("Attribute11_Scaled", "Attribute11_CATA", data)

#manage_cases()


test_that("makePairwisePlot generates a list", {
  expect_match(typeof(makePairwisePlot("Attribute11_Scaled", "Attribute12_Scaled", data)), "list")
})

test_that("makePairwisePlot generates a plot", {
  expect_doppelganger(title="rplot001", makePairwisePlot("Attribute11_Scaled", "Attribute12_Scaled", data))
})



# test_that("elements of the list are correct",{
#   expect_equal()
#
# test_that("makePairwisePlot generates a plot", {
#   expect_doppelganger(title = "test plot", fig = plot, path = file.path("tests", "fig"))
# })
#
