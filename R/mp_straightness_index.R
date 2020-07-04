#' Computes the straightness index for the trajectory between the first and last campsite location in a Track object.
#'
#' \code{mp_straightness_index} computes the straightness index
#' \insertCite{Laube.2007}{herdersmovementpatterns} for the trajectory between the first and last locations of a
#' \code{Track} object in the order of movement.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @return a numeric value representing the straightness index for
#' \code{t}.
#' @references
#'    \insertAllCited{}
#' @examples #
#' @export
mp_straightness_index <- function(t) {

  # checks
  if(!(inherits(t, "Track"))) {
    stop("t must be a Track object\n")
  }
  if(!(any(colnames(t@data) == "location") && any(colnames(t@data) == "campsite"))) {
    stop("the t@data must contain a variable 'location' and 'campsite'\n")
  }

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- t@data
  index <- which(sel$location != 0 & sel$campsite)
  index <- index[!duplicated(sel$location[index])]

  # extract the corresponding values and create a new Track
  newt1 <- trajectories::Track(track = spacetime::STIDF(sp = t@sp[index,],
                                                        time = t@time[index],
                                                        data = sel[index,],
                                                        endTime = t@time[index]))
  newt2 <- trajectories::Track(track = spacetime::STIDF(sp = t@sp[index[c(1, length(index))],],
                                                        time = t@time[index[c(1, length(index))]],
                                                        data = sel[index[c(1, length(index))],],
                                                        endTime = t@time[index[c(1, length(index))]]))

  # compute the average distances
  sum(newt1@connections$distance)/sum(newt2@connections$distance)

}
