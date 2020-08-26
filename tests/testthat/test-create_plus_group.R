context("create_plus_group")

test_that("function exists", {
  expect_is(create_plus_group, "function")
})

test_that("function rounds upper age as plus group", {
  expect_equal(
  al_sample %>%
    dplyr::mutate(Age = create_plus_group(Age, plus_group = 5)) %>%
    dplyr::pull(Age) %>%
    unique(),
  c(1, 2, 3, 4, 5)
  )
})
