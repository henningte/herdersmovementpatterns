#' Clips \code{TrackCollections} to specified time windows
#'
#' \code{tc_clip} clips the \code{Track} objects of
#' a \code{TracksCollection} object for
#' specified time windows.
#'
#' @param tc An object of class
#' \code{\link[trajectories:Track-class]{TracksCollection}} with one
#' \code{Track} per \code{Tracks} and the same length and time resolution
#' for each \code{Track} object.
#' @param d A data.frame with a row for each time window and two columns:
#' \describe{
#'   \item{\code{start}}{A \code{Date} vector with the start time for each
#'   time window.}
#'   \item{\code{end}}{A \code{Date} vector with the end time for each
#'   time window.}
#' }
#' @return A list with one \code{{TracksCollection}} for each row in \code{d}
#' representinga clipped form of \code{tc}.
#' @seealso .
#' @examples #
#' @export
tc_clip <- function(tc,
                    d) {

  # checks
  stopifnot(inherits(tc, "TracksCollection"))
  stopifnot(inherits(d, "data.frame"))
  stopifnot(ncol(d) == 2)

  # define clipping indices
  tc_time <- as.Date(as.POSIXct(tc@tracksCollection[[1]]@tracks[[1]]@time))
  d_index <- apply(d, 2, function(x) sapply(x, function(y) which(tc_time == y)))

  # for each time block
  lapply(seq_len(nrow(d_index)), function(x){

    # convert the start and end indices into a sequence
    index <- d_index[x,1]:d_index[x,2]

    # clip the trajectories
    tc_cliped <- trajectories::TracksCollection(lapply(tc@tracksCollection, function(t){

      t <- t@tracks[[1]]

      trajectories::Tracks(list(trajectories::Track(track = spacetime::STIDF(sp = t@sp[index,],
                                                                             time = t@time[index],
                                                                             data = t@data[index,],
                                                                             endTime = t@time[index]))))

    }))

    # restore track names
    names(tc_cliped@tracksCollection) <- names(tc@tracksCollection)
    tc_cliped

  })

}
