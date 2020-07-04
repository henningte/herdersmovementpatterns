#' Computes the mean distance between campsite locations of a Track object in the order of movement.
#'
#' \code{mp_locations_distance_average} computes the mean distance between
#' locations of a \code{Track} in the order of movement.
#' Several options are available.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @return a numeric value representing the number of unique (campsite) locations
#' in \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_locations_distance_average <- function(t,
                                          fun = stats::median) {

  # checks
  if(!(inherits(t, "Track"))) {
    stop("t must be a Track object\n")
  }
  if(!(any(colnames(t@data) == "location") && any(colnames(t@data) == "campsite"))) {
    stop("the t@data must contain a variable 'location' and 'campsite'\n")
  }
  if(!(is.function(fun))) {
    stop("fun must be a function")
  }

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- t@data
  index <- which(sel$location != 0 & sel$campsite)
  index <- index[!duplicated(sel$location[index])]

  # extract the corresponding values and create a new Track
  newt <- trajectories::Track(track = spacetime::STIDF(sp = t@sp[index,],
                                               time = t@time[index],
                                               data = sel[index,],
                                               endTime = t@time[index]))

  # compute the average distances
  fun(newt@connections$distance)

}
