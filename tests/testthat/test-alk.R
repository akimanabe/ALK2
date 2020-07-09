context("forward alk")

test_that("forward_alk properly consume data.frame", {
  foo <- tibble::tibble(x = seq(1:10), y = seq(1:10) / 10)
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
  foo <- tibble::tibble(x = seq(1:10), y = seq(1:10) / 10)
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

context("create_forward_alk")

test_that("function creates alk properly", {
  expect_is(create_forward_alk(sampledata),
            "data.frame")

  expect_error(
  sampledata %>%
    dplyr::rename(age = Age,
                  len = Length) %>%
    create_forward_alk()
  )

  expect_equal(
    create_forward_alk(sampledata) %>%
      colnames(.) %>%
      .[1], "Age"
  )

  expect_true(
    create_forward_alk(sampledata) %>%
      colnames(.) %>%
      .[-1] %>%
      as.character() %>%
      stringr::str_detect(., "\\d") %>%
      unique()
    )

  expect_equal(
    create_forward_alk(sampledata) %>%
    tidyr::pivot_longer(cols = (-Age),
                        names_to = "Length",
                        values_to = "Proportion") %>%
    dplyr::group_by(Length) %>%
    dplyr::summarise(Psum = sum(Proportion, na.rm = TRUE)) %>%
    dplyr::pull(Psum) %>%
      unique(), 1)

})

context("create_reverse_alk")

test_that("function creates reverse alk properly", {
  expect_is(create_reverse_alk, "function")
  expect_is(create_reverse_alk(sampledata),
            "data.frame")

  expect_error(
    sampledata %>%
      dplyr::rename(age = Age,
                    len = Length) %>%
      create_reverse_alk()
  )

  expect_equal(
    create_reverse_alk(sampledata) %>%
      colnames(.) %>%
      .[1], "Age"
  )

  expect_true(
    create_reverse_alk(sampledata) %>%
      colnames(.) %>%
      .[-1] %>%
      as.character() %>%
      stringr::str_detect(., "\\d") %>%
      unique()
  )

  expect_equal(
    create_reverse_alk(sampledata) %>%
      tidyr::pivot_longer(cols = (-Age),
                          names_to = "Length",
                          values_to = "Proportion") %>%
      dplyr::group_by(Age) %>%
      dplyr::summarise(Psum = sum(Proportion, na.rm = TRUE)),
    tibble::tibble(Age = seq(0, max(sampledata$Age))) %>%
      dplyr::mutate(Psum = rep(1))
    )
})

context("apply_forward_alk")

test_that("apply_fowrad_alk functions properly", {

  expect_is(apply_forward_alk, "function")

  ldata <-
    sampledata %>%
    dplyr::select(Length) %>%
    count_ldata()

  expect_is(apply_forward_alk(sampledata, ldata), "data.frame")

  expect_equal(
  apply_forward_alk(sampledata, ldata) %>%
    dplyr::group_by(Age) %>%
    dplyr::summarise(Fishsum = sum(Fish, na.rm = T)) %>%
    dplyr::pull(Fishsum),

  sampledata %>%
    dplyr::select(Age) %>%
    dplyr::count(Age) %>%
    dplyr::pull(n)
  )

})
