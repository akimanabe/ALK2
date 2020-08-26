context("limit_length_bin")

test_that("function exists", {
  expect_is(limit_length_bin, "function")
})

test_that("function limits numbers properly", {
  expect_equal(limit_length_bin(freq_sample, c(10, 30)) %>%
                 dplyr::group_by(Length) %>%
                 dplyr::summarise() %>%
                 dplyr::pull() %>%
                 min(),
               10
                 )

  expect_equal(limit_length_bin(freq_sample, c(10, 30)) %>%
                 dplyr::group_by(Length) %>%
                 dplyr::summarise() %>%
                 dplyr::pull() %>%
                 max(),
               30
  )
})
