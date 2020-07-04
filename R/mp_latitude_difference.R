#' Computes the latitude difference between the most northern and most southern points of a Track object.
#'
#' \code{mp_latitude_difference} computes the latitudinal difference between
#' the most northern and most southern points of a
#' \code{Track}.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric value representing the latitudinal difference between the
#' most northern and most southern points in \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_latitude_difference <- function(t) {

  # compute the latitude difference
  diff(range(t@sp@coords[,2]))

}
