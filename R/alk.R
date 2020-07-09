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

#' Create ALK by length
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#'
#' @examples
#' \dontrun{
#' forward_alk(sampledata)
#' }
forward_alk <- function(dat) {

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

#' Create forward alk
#'
#' @param dat tibble with two columns with colnames = c("Length", "Age")
#'
#' @return ALK data with n (frequency) and prop (proportion)
#' @export
#'
#' @examples
#' \dontrun{
#' create_forward_alk(sampledata)
#' }
create_forward_alk <- function(dat) {
    dat %>%
    tidyr::drop_na() %>%
    forward_alk() %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = Length, values_from = prop)
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

#' Apply forward alk to length frequency data
#'
#' @param aldata tibble with age and length data with
#' colnames = c("Length", "Age")
#' @param lengthdata tibble with length and frequency data with
#' colnames = c("Length", "Frequency")
#'
#' @return tibble with alk applied to the frequency data
#' @export
#'
#' @examples
#' \dontrun{
#' mydata <- sampledata
#' mylfreq <- sampledata %>% dplyr::select(Length) %>% count_ldata(.)
#'
#' apply_forward_alk(mydata, mylfreq)
#' }
apply_forward_alk <- function(aldata, lengthdata) {
  assertthat::assert_that(
    assertthat::are_equal(colnames(aldata), c("Length", "Age"))
  )
  assertthat::assert_that(
    assertthat::are_equal(colnames(lengthdata), c("Length", "Frequency"))
  )

  aldata %>%
    tidyr::drop_na() %>%
    create_forward_alk() %>%
    tidyr::pivot_longer(cols = c(-Age),
                        names_to = "Length",
                        values_to = "Proportion") %>%
    dplyr::mutate(Length = as.double(Length)) %>%
    dplyr::inner_join(., lengthdata, by = "Length") %>%
    dplyr::mutate(Fish = Proportion * Frequency) %>%
    dplyr::select(Age, Length, Fish)
}
