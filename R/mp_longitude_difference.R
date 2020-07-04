#' Computes the longitude difference between the most western and most eastern points of a Track object.
#'
#' \code{mp_longitude_difference} computes the longitude difference between
#' the most western and most eastern points of a
#' \code{Track}.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}.
#' @return a numeric value representing the longitudinal difference between the
#' most western and most eastern points in \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_longitude_difference <- function(t) {

  # compute the longitude difference
  diff(range(t@sp@coords[,1]))

}
