#' Computes the proportion of gaps in a Track object.
#'
#' \code{mp_gap_proportion} computes the proportion of gaps in a
#' \code{Track}.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location}.
#' @return A numeric value representing the proportion of gaps
#' in \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_gap_proportion <- function(t) {

  # checks
  if(!(inherits(t, "Track"))) {
    stop("t must be a Track object\n")
  }
  if(!(any(colnames(t@data) == "location"))) {
    stop("fun must be a function")
  }

  # compute the proportion of gaps
  length(which(t@data$location == 0))/nrow(t@data)

}
