sampledata <- readr::read_csv("./tests/testthat/excel/dummy_population.csv")

usethis::use_data(sampledata, overwrite = TRUE)
