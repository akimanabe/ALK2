#' Create ALK by age
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#' @export
#'
#' @examples
#' \dontrun{
#' inverse_alk(sampledata)
#' }
inverse_alk <- function(dat) {

  assertthat::assert_that(
    assertthat::are_equal(colnames(dat), c("Length", "Age"))
    )

  dat %>%
    dplyr::count(Age, Length) %>%
    dplyr::group_by(Age) %>%
    dplyr::mutate(prop = prop.table(n))
}

#' Create inverse alk
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#' @export
#'
#' @examples
#' \dontrun{
#' create_inverse_alk(sampledata)
#' }
create_inverse_alk <- function(dat) {
  dat %>%
    tidyr::drop_na() %>%
    inverse_alk() %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = Length, values_from = prop)
}
