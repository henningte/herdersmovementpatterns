#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
NULL

#' Extracts information on gaps from a \code{data.frame} representing a \code{Trackscollection} object.
#'
#' \code{mp_tcdf_extract_gaps} extracts information on gaps from a
#' \code{\link[trajectories:Track-class]{TracksCollection}} object
#' that was combined using \code{\link{mp_tc_rbind}}.
#'
#' @param tcdf A \code{data.frame} as returned by \code{\link{mp_tc_rbind}}
#' if applied on a \code{\link[trajectories:Track-class]{TracksCollection}}
#' object.
#' @param dummy A logical value indicating if in the case that for a household
#' no gap is detected, a dummy entry should be inserted in the resulting
#' \code{data.frame} (\code{dummy = TRUE}) or not (\code{dummy = FALSE}).
#' @return a \code{data.frame} object with the following variables:
#' \describe{
#'   \item{\code{household}}{A numeric vector with an id for each household.}
#'   \item{\code{location}}{A numeric vector with an id for each location per
#'   household.}
#'   \item{\code{group}}{A numeric vector with an id for each visit per
#'   household and location.}
#'   \item{\code{row_start}}{A numeric vector with an id for the row of
#'   \code{tcdf}, where a gap has its first value.}
#'   \item{\code{row_end}}{A numeric vector with an id for the row of
#'   \code{tcdf}, where a gap has its last value.}
#'   \item{\code{time_start}}{A \code{Date} vector with the start date of
#'   each gap.}
#'   \item{\code{time_end}}{A \code{Date} vector with the end date of
#'   each gap.}
#'   \item{\code{duration}}{A numeric vector with the duration of each gap
#'   in days.}
#'   \item{\code{is_first}}{A logical vector indicating for each gap if it is
#'   the first gap for a household.}
#'   \item{\code{is_last}}{A logical vector indicating for each gap if it is
#'   the last gap for a household.}
#'   \item{\code{is_gap}}{A logical vector indicating if the current row represents
#'   a gap (always \code{TRUE}).}
#'   \item{\code{location_previous}}{A numeric vector with an id for the location
#'   prior the gap.}
#'   \item{\code{location_next}}{A numeric vector with an id for the location
#'   after the gap.}
#'   \item{\code{moved}}{A logical vector indicating for each gap if a movement
#'   occured during the gap. This is always the case if
#'   \code{location_previous != location_next}.}
#'   \item{\code{proportion}}{A numeric vector with the temporal proportion of each gap
#'   relative to the duration of the trajectory of the household.}
#' }
#' @seealso .
#' @examples #
#' @export
mp_tcdf_extract_gaps <- function(tcdf, dummy = TRUE){

  # checks
  stopifnot(inherits(tcdf, "data.frame"))

  # get the first and last day in the TracksCollection
  tc_time_range <- range(tcdf$time)

  # get the total duration of one track
  tc_duration <- abs(difftime(tc_time_range[1], tc_time_range[2]))

  # extract and merge the data.frames for each household
  hh <- tcdf

  # add a row number index
  hh$row <- seq_len(nrow(hh))

  # change groups for gaps
  hh$group[hh$location == 0] <- data.table::rleidv(hh$group)[hh$location == 0]

  # remove all non-gap values
  # hh <- hh[hh$gap,]

  # group by households and gap
  hh1 <- dplyr::group_by(hh, .data$household, .data$location, .data$group)
  hh1 <- dplyr::summarise(hh1,
                          row_start = dplyr::first(row),
                          row_end = dplyr::last(row),
                          time_start = dplyr::first(.data$time),
                          time_end = dplyr::last(.data$time),
                          duration = as.numeric(difftime(dplyr::last(.data$time), dplyr::first(.data$time))),
                          is_first = ifelse(any(.data$time %in% tc_time_range[1]), TRUE, FALSE),
                          is_last = ifelse(any(.data$time %in% tc_time_range[2]), TRUE, FALSE),
                          is_gap = ifelse(.data$gap[1], TRUE, FALSE))
  hh1 <- hh1[order(hh1$household, hh1$time_start),]

  # add information on previous and next locations
  hh1$location_previous <- ifelse(hh1$is_first, NA, c(NA, hh$location[hh1$row_start-1]))
  hh1$location_next <- ifelse(hh1$is_last, NA, c(hh$location[hh1$row_end+1], NA))

  # detect moves (if a gap represents a move)
  hh1$moved <- ifelse(hh1$location_previous != hh1$location_next, TRUE, FALSE)
  hh1$moved[!hh1$is_gap] <- FALSE

  # compute the proportion of gaps
  hh1$proportion <- as.numeric(hh1$duration)/as.numeric(tc_duration)

  # remove all non-gap values
  hh1 <- hh1[hh1$is_gap,]

  if(dummy){

    # add dummy entries for missing households
    missing_households <- unique(hh$household)[which(!unique(hh$household) %in% hh1$household)]
    missing_households_n <- length(missing_households)

    if(missing_households_n > 0){

      missing_households <- data.frame(
        household = rep(missing_households, each = 2),
        location = NA,
        group = NA,
        row_start = NA,
        row_end = NA,
        time_start = NA,
        time_end = NA,
        duration = 0,
        is_first = rep(c(TRUE, FALSE), missing_households_n),
        is_last = rep(c(FALSE, TRUE), missing_households_n),
        is_gap = TRUE,
        location_previous = NA,
        location_next = NA,
        moved = FALSE,
        proportion = 0
      )

      # add dummy values to hh1
      hh1 <- dplyr::bind_rows(hh1, missing_households)

    }

  }

  # return hh1
  hh1[order(hh1$household, hh1$location, hh1$group),]

}
