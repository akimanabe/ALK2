context("categorize_num")

test_that("categorize_num properly categorize numerical vector", {
  expect_is(categorize_num, "function")
  expect_equal(categorize_num(c(0.5, 1.2, 2.3, 4.0)), c(0, 1, 2, 4))
  expect_equal(categorize_num(c(20.2, 22.5, 22.3, 16.4, 19.1, 22.8, 23.9),
                              interval = 0.5),
               c(20, 22.5, 22, 16, 19, 22.5, 23.5))
})


context("subsample_data")

test_that("subsample_data returns lengthy data", {
  expect_equal(nrow(subsample_data(sampledata)), 2143)
  expect_equal(nrow(subsample_data(sampledata, sub_prop = 0.5)), 2143)
})

test_that("subsample Age according to given proportion", {

  expect_equal(
    subsample_data(sampledata, sub_prop = 0.4) %>%
      tidyr::drop_na() %>%
      nrow(),
    2143 * 0.4,
    tolerance = 1
  )

  expect_equal(
    subsample_data(sampledata, sub_prop = 0.1) %>%
      tidyr::drop_na() %>%
      nrow(),
    2143 * 0.1,
    tolerance = 1
  )

})
