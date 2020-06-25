#' Create ALK by age
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#' @export
#'
#' @examples
#' \dontrun{
#' forrward_alk(sampledata)
#' }
forward_alk <- function(dat) {

  assertthat::assert_that(
    assertthat::are_equal(colnames(dat), c("Length", "Age"))
    )

  dat %>%
    dplyr::count(Age, Length) %>%
    dplyr::group_by(Age) %>%
    dplyr::mutate(prop = prop.table(n))
}

#' Create ALK by length
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#' @export
#'
#' @examples
#' \dontrun{
#' reverse_alk(sampledata)
#' }
reverse_alk <- function(dat) {

  assertthat::assert_that(
    assertthat::are_equal(colnames(dat), c("Length", "Age"))
  )

  dat %>%
    dplyr::count(Age, Length) %>%
    dplyr::group_by(Length) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::arrange(Length) %>%
    dplyr::select(Length, Age, n, prop)

}
