#' Limit length bin by applying plus and minus group
#'
#' @param length_data Length frequency data
#' @param limit_length Limit of minus and plus group
#' given as \code{c(lower_limit, upper_limit)}
#'  \describe{
#'   \item{lower_limit}{lower limit of length bin}
#'   \item{upper_limit}{upper limit of length bin}
#'  }
#' @export
#' @examples
#' \dontrun{
#' limit_length_bin(Length = freq_sample, limit_length = c(10, 30))
#' }
limit_length_bin <- # nolint
  function(length_data, limit_length = c(NA, NA)) {
    length_data %>%
      dplyr::arrange(Length) %>%
      dplyr::mutate(Length = dplyr::if_else(Length < limit_length[1],
                                            limit_length[1], Length),
                    Length = dplyr::if_else(Length > limit_length[2],
                                            limit_length[2], Length))
  }
