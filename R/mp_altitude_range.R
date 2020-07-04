#' Extracts the altitude range from a Track object.
#'
#' \code{mp_altitude_range} extracts the altitude range
#' \code{Track} object with a variable
#' \code{altitude} in the data slot. \code{NA}s are removed.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric vector of length two with the range of the altitude values
#' of \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_altitude_range <- function(t) {

  # checks
  if(!(inherits(t, "Track"))) {
    stop("t must be a Track object\n")
  }
  if(!(any(colnames(t@data) == "altitude"))) {
    stop("the t@data must contain a variable 'altitude'\n")
  }

  # compute the altitude range
  range(t$altitude, na.rm = TRUE)

}
