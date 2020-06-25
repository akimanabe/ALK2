context("categorize_num")

test_that("categorize_num properly categorize numerical vector", {
  expect_is(categorize_num, "function")
  expect_equal(categorize_num(c(0.5, 1.2, 2.3, 4.0)), c(0, 1, 2, 4))
  expect_equal(categorize_num(c(20.2, 22.5, 22.3, 16.4, 19.1, 22.8, 23.9),
                              interval = 0.5),
               c(20, 22.5, 22, 16, 19, 22.5, 23.5))
})
