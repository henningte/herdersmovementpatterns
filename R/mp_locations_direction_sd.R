#' Computes the standard eviation of the direction (as absolute difference of angles) between campsite locations of a Track object in the order of movement.
#'
#' \code{mp_locations_direction_sd} computes the standard deviation of directions
#' between locations of a \code{Track} in the order of movement.
#' Directions are computed as angle difference between the previous campsite location
#' and the current campsite location and the next campsite location and the current
#' campsite location. Several options are available.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}.
#' @return a numeric value representing the standard deviation of directions between
#' campsites in the order of movement.
#' @seealso .
#' @examples #
#' @export
mp_locations_direction_sd <- function(t) {

  # checks
  stopifnot(inherits(t, "Track"))

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- t@data
  index <- which(sel$location != 0 & sel$campsite)
  index <- index[!duplicated(sel$location[index])]

  # extract the corresponding values and create a new Track
  newt <- trajectories::Track(track = spacetime::STIDF(sp = t@sp[index,],
                                                                  time = t@time[index],
                                                                  data = sel[index,],
                                                                  endTime = t@time[index]))

  # compute the standard deviation of directions
  stats::sd(angdiff(newt@connections$direction[-1], newt@connections$direction[-nrow(newt@connections)]), na.rm = TRUE)

}
