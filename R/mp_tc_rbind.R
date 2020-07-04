#' Binds the data slots of \code{Track} objects in a \code{Trackscollection} object row-wise.
#'
#' \code{mp_tc_rbind} rowwise binds the data slots of \code{Track} objects
#' in a \code{TracksCollection} object.
#'
#' @param tc A \code{\link[trajectories:Track-class]{TracksCollection}} object
#' with all \code{Track} objects having the same length and temporal resolution.
#' @return a \code{data.table} object with the row-wise binded data slots of the
#' \code{Track} objects of \code{tc} and two additional variables, \code{household}
#' (a numeric vector with an id for each household) and \code{time} (a \code{Date}
#' vector with the date information of each \code{Track} object).
#' @seealso .
#' @examples #
#' @export
mp_tc_rbind <- function(tc) {

  # checks
  stopifnot(inherits(tc, "TracksCollection"))

  # extract and merge the data.frames for each household
  hh <- lapply(seq_along(tc@tracksCollection), function(x){
    cbind(household = x,
          time = as.Date(as.POSIXct(tc@tracksCollection[[x]]@tracks[[1]]@time)),
          tc@tracksCollection[[x]]@tracks[[1]]@data)
  })
  data.table::rbindlist(hh)

}
