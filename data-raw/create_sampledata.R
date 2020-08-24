sampledata <- readr::read_csv("./tests/testthat/excel/dummy_population.csv")

usethis::use_data(sampledata, overwrite = TRUE)


freq_sample <-
  dplyr::sample_n(sampledata, 2000, replace = FALSE) %>%
  dplyr::select(Length)

usethis::use_data(freq_sample, overwrite = TRUE)

al_sample <-
  dplyr::sample_n(sampledata, 400, replace = TRUE) %>%
  dplyr::arrange(., Length)

usethis::use_data(al_sample, overwrite = TRUE)
