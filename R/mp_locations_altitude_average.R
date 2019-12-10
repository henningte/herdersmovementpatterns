#' @importFrom Rdpack reprompt
#' @importFrom trajectories Track
NULL

#' Computes the average altitude of locations of a Track object.
#'
#' \code{mp_locations_altitude_average} computes the average altitude of lcoations of a
#' \code{\link[trajectories:Track-class]{Track}}. Several options are available. For each
#' location, only one value is considered (i.e. thetemporal duration of visits is not
#' considered as weights during computations).
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} and a variable \code{location} and \code{campsite}
#' as created by \code{\link[herdersTA]{locationsTrack}} in the data slot.
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @param campsite A logical value indicating if only campsites should be considered
#' (\code{TRUE}) or any locations (\code{FALSE}).
#' @return a numeric value representing the average altitude of the locations in \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_locations_altitude_average <- function(t, fun = stats::median, campsite = TRUE){

  # checks
  if(!(inherits(t, "Track"))) {
    stop("t must be a Track object\n")
  }
  if(!(any(colnames(t@data) == "altitude") && any(colnames(t@data) == "location") && any(colnames(t@data) == "campsite"))) {
    stop("the t@data must contain a variable 'altitude', 'location' and 'campsite'\n")
  }
  if(!(is.function(fun))) {
    stop("fun must be a function")
  }
  if(!(is.logical(campsite) && length(campsite) == 1)) {
    stop("campsite must be a logical value\n")
  }

  # remove duplicated values and gaps
  sel <- t@data
  index <- which(sel$location != 0)
  index <- index[!duplicated(sel$location[index])]
  sel <- sel[index,]

  # remove locations related to short-term visits
  if(campsite) {
    sel <- sel[sel$campsite,]
  }

  # compute the average value
  fun(sel$altitude)

}
