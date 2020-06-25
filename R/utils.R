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
