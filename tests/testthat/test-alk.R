context("forward alk")

test_that("forward_alk properly consume data.frame", {
  foo <- tibble::tibble(x = seq(1:10), y = seq(1:10)/10)
  bar <- foo %>%
    dplyr::mutate(Length = as.factor(x),
                  Age = as.factor(y))

  expect_error(forward_alk(foo))
  expect_error(forward_alk(bar))

  expect_is(forward_alk(sampledata), "data.frame")
  expect_gte(forward_alk(sampledata) %>%
               nrow(), 1)
  expect_gte(forward_alk(sampledata) %>%
               ncol(), 4)
})

context("reverse alk")

test_that("reverse properly consume data.frame", {
  foo <- tibble::tibble(x = seq(1:10), y = seq(1:10)/10)
  bar <- foo %>%
    dplyr::mutate(Length = as.factor(x),
                  Age = as.factor(y))

  expect_error(reverse_alk(foo))
  expect_error(reverse_alk(bar))

  expect_is(reverse_alk(sampledata), "data.frame")
  expect_gte(reverse_alk(sampledata) %>%
               nrow(), 1)
  expect_gte(reverse_alk(sampledata) %>%
               ncol(), 4)
})
