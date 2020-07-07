#' Categorize numerical vector to size or age class
#'
#' @param data numeric vector
#' @param interval interval of size or age class
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' \dontrun{
#' categorize_num(1:10, 1)
#' categorize_num(runif(10), 0.1)
#' }
categorize_num <- function(data, interval = 1) {
  floor(data / interval) * interval
}

#' Subsample sampledata
#'
#' @param dat sampledata
#' @param sub_prop Proportion of data equipped with age
#'
#' @return sampledata with and without age
#' @export
#'
#' @examples
#' \dontrun{
#' subsample_data(subsample, sub_prop = 0.5)
#' }
subsample_data <-
  function(dat, sub_prop = 0.25) {
    age_length_data <-
      dat %>%
      dplyr::mutate(rownum = seq_len(nrow(dat))) %>%
      dplyr::slice_sample(prop = sub_prop)

    length_data <-
      dplyr::anti_join(
        dat %>%
          dplyr::mutate(rownum = seq_len(nrow(dat))) %>%
          dplyr::mutate(Age = NA_real_),
        age_length_data,
        by = "rownum"
      )

    dplyr::bind_rows(age_length_data, length_data) %>%
      dplyr::select(-rownum) %>%
      dplyr::arrange(Age, Length)
  }

#' Create length frequency data
#'
#' @param dat single strand vector with length data
#'
#' @return length frequency
#' @export
#'
#' @examples
#' \dontrun{
#' ldata <- tibble::tibble(Length = round(runif(1000, 5, 40)))
#' count.ldata(ldata)
#' }
count.ldata <- function(dat) {
  dat %>%
    dplyr::group_by(Length) %>%
    dplyr::count(Length) %>%
    dplyr::rename(Frequency = n)
}
