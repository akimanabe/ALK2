#' Create ALK by age
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
    dplyr::group_by(Age) %>%
    dplyr::mutate(prop = prop.table(n))
}

#' Create reverse alk
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#' @export
#'
#' @examples
#' \dontrun{
#' create_reverse_alk(sampledata)
#' }
create_reverse_alk <- function(dat) {
  dat %>%
    tidyr::drop_na() %>%
    reverse_alk() %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = Length, values_from = prop)
}
