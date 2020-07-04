#' Computes the total altitude distance covered by a Track object.
#'
#' \code{mp_altitude_total_distance} computes the total altitude distance
#' covered by a \code{Track} (sum of absolute altitude
#' distances).
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the \code{data} slot.
#' @return a numeric value representing the total altitudinal distance covered by
#' \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_altitude_total_distance <- function(t) {

  # compute the latitude difference
  sum(abs(t@data$altitude[-1] - t@data$altitude[-length(t)]))

}
