#' Nicely prints sequences of numbers.
#'
#' \code{mp_number_sequence} takes a numeric vector and nicely prints its elements
#' as sequence.
#'
#' @param x A numeric vector with at least two elements.
#' @param digits An integer vale representing the digit to which the
#' elements of \code{x} should be rounded.
#' @return A string representing \code{x} as nicely formatted sequence.
#' @examples
#' x <- c(23.345, 232.4234, 12.3)
#' mp_number_sequence(x, digits = 1)
#' @export
mp_number_sequence <- function(x,
                               digits = 2) {

  if(!is.numeric(x)) {
    stop("`x` must be numeric.")
  }
  if(length(x) < 2) {
    stop("`x` must have at least two elements.")
  }
  if(!is.numeric(digits)) {
    stop("`digits` must be numeric.")
  }
  if(length(digits) != 1) {
    stop("`digits` must be of length 1.")
  }

  x <- round(x, digits = digits)
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])

}
