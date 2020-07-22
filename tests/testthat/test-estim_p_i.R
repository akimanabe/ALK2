context("estim_p_i")

test_that("estim_p_i properly returns proportion of age i", {
  expect_is(estim_p_i, "function")
  expect_error(estim_p_i(c(1, 2, 3)))
  expect_is(estim_p_i(sampledata),
            "data.frame")
  expect_equal(colnames(estim_p_i(sampledata)),
               c("Age", "p_i"))
  expect_equal(estim_p_i(sampledata) %>%
                 dplyr::pull(p_i) %>%
                 sum(., na.rm = TRUE),
               1)
})
