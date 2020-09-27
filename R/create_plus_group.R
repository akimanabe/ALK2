#' Create plus group of age
#'
#' @param age Age data
#' @param plus_group integer age which floors upper age as plus group
#' @export
create_plus_group <-
  function(age, plus_group) {
    age %>%
      dplyr::mutate(age = dplyr::if_else(age > plus_group, plus_group, age))
  }

# al_sample %>%
#   dplyr::select(Age) %>%
#   create_plus_group(., 5)
