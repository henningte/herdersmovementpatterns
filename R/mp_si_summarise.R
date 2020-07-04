#' Computes the mean, median, minimum, maximum, and number of samples in a vector.
#'
#' \code{mp_si_summarise} takes a numeric vector and computes its mean, median, minimum,
#' maximum, and number of non-\code{NA} elements, and rounds the results.
#'
#' @param x A numeric vector.
#' @param digits An integer vale representing the digit to which the
#' elements of \code{x} should be rounded.
#' @return A \code{data.frame} with one row and a column for each the
#' mean (mean), median (median), minimum (min), maximum (max), and number of samples (n).
#' @examples
#' x <- c(23.345, 232.4234, 12.3, 23423.23, 456, 23)
#' mp_si_summarise(x, digits = 1)
#' @export
mp_si_summarise <- function(x,
                            digits = 2) {

  # compute values
  si_mean <- round(mean(x, na.rm = TRUE), digits)
  si_median <- round(stats::median(x, na.rm = TRUE), digits)
  si_min <- round(min(x, na.rm = TRUE), digits)
  si_max <- round(max(x, na.rm = TRUE), digits)
  si_n <- length(stats::na.omit(x))

  # create data.frame
  data.frame(mean = si_mean, median = si_median, min = si_min, max = si_max, n = si_n)
}
