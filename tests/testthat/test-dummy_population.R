context("Dummy population")

test_that("data exists", {
  data <- readr::read_csv("./excel/dummy_population.csv")
  expect_is(data, "data.frame")
  expect_equal(nrow(data), 2143)
  expect_equal(colnames(data), c("Length", "Age"))
})
