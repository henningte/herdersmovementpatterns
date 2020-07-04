#' Computes the total altitude distance covered by a Track object.
#'
#' \code{mp_altitude_total_relative_distance} computes the total altitude distance
#' covered by a \code{Track} (sum of absolute altitude
#' distances) divided by the total horizontal distance covered by the same track.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the \code{data} slot.
#' @return a numeric value representing the total altitude distance covered by
#' \code{t}, relative to the total horizontal distance covered by t.
#' @seealso .
#' @examples #
#' @export
mp_altitude_total_relative_distance <- function(t) {

  # compute the altitude distance
  mp_altitude_total_distance(t)/mp_distance_total(t)

}
