#' @importFrom Rdpack reprompt
#' @importFrom trajectories Track
#' @importFrom spacetime STIDF
NULL

#' Computes the average direction (as absolute difference of angles) between campsite locations of a Track object in the order of movement.
#'
#' \code{mp_locations_direction_average} computes the average direction between
#' locations of a \code{\link[trajectories:Track-class]{Track}} in the order of movement.
#' Directions are computed as angle difference between the previous campsite location
#' and the current campsite location and the next campsite location and the current
#' campsite location. Several options are available.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @return a numeric value representing the average direction between
#' locations in \code{t} in the order of movement.
#' @seealso .
#' @examples #
#' @export
mp_locations_direction_average <- function(t, fun = stats::median){

  # checks
  stopifnot(inherits(t, "Track"))
  stopifnot(is.function(fun))

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- t@data
  index <- which(sel$location != 0 & sel$campsite)
  index <- index[!duplicated(sel$location[index])]

  # extract the corresponding values and create a new Track
  newt <- trajectories::Track(track = spacetime::STIDF(sp = t@sp[index,],
                                                       time = t@time[index],
                                                       data = sel[index,],
                                                       endTime = t@time[index]))

  # compute the average directions
  fun(angdiff(newt@connections$direction[-1], newt@connections$direction[-nrow(newt@connections)]), na.rm = TRUE)

}
