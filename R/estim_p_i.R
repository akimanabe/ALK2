#' Estimate proportion of age within the whole sample
#'
#' @param dat data
#'
#' @return p_i proportion of age i in sample n
#' @export
estim_p_i <- function(dat) {
  assertthat::assert_that(
    assertthat::are_equal(colnames(dat), c("Length", "Age"))
  )
  dat %>%
    dplyr::group_by(Age) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(p_i = n / nrow(dat)) %>%
    dplyr::select(Age, p_i)

}
