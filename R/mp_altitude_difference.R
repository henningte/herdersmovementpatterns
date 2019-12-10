#' @importFrom Rdpack reprompt
NULL

#' Computes the altitude difference between the highest and lowest point of a Track object.
#'
#' \code{mp_altitude_difference} computes the altitude difference between
#' the highest and the lowest point of a
#' \code{\link[trajectories:Track-class]{Track}} object with a variable
#' \code{altitude} in the data slot. \code{NA}s are removed.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric value representing the altitude difference between the
#' highest and the lowest point in \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_altitude_difference <- function(t){

  # compute the altitude difference
  diff(mp_altitude_range(t))

}
