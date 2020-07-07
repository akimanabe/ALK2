generate.lengthdata <- function(dat = sampledata){
  subsample_data(dat, sub_prop = 0.8) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(Age = NA_real_)
}

generate.alsample <- function(dat = sampledata){
  subsample_data(dat, sub_prop = 0.2) %>%
    tidyr::drop_na()
}

count.ldata <- function(dat) {
  dat %>%
    dplyr::group_by(Length) %>%
    dplyr::count(Length) %>%
    dplyr::rename(Frequency = n)
}
