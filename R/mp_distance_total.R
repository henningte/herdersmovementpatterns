#' @importFrom Rdpack reprompt
#' @importFrom trajectories Track
NULL

#' Computes the total distance covered by a Track object.
#'
#' \code{mp_distance_total} computes the total distance covered by a
#' \code{\link[trajectories:Track-class]{Track}}. This is the sum of all distance
#' values in the \code{connections} slot.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}.
#' @return a numeric value representing the total distance covered by \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_distance_total <- function(t){

  # compute the total horizontal distance
  sum(t@connections$distance)

}
