#' Estimates the linearity of a Track object.
#'
#' \code{mp_linearity} computes an estimate of the
#' linearity of a trajectory (object of class \code{Track})
#' by (1) extracting the maximum distance between any two locations,
#' (2) extracting the maximum distance between any two remaining locations
#' orthogonal to the first line and (3) dividing the values extracted in (1) and (2).
#' The larger the value is the more linear is the trajectory overall.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @return a numeric value representing the straightness index for
#' \code{t}.
#' @seealso .
#' @examples #
#' @export
mp_linearity <- function(t) {

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

  # extract the locations
  sel_locations <- t@sp[index,]

  # compute distance between points
  sel_locations_distances <- sp::spDists(sel_locations, longlat = FALSE)

  # get maximum distance
  sel_locations_distance_max1 <- which.max(apply(sel_locations_distances, 2, max))
  sel_locations_distance_max2 <- which.max(sel_locations_distances[,sel_locations_distance_max1])
  sel_locations_distance_max <- max(sel_locations_distances)

  # construct a line between point 1 and 2
  sel_locations_distance_line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(sel_locations[c(sel_locations_distance_max1, sel_locations_distance_max2),]), ID = "a")))

  # compute distance between points and line
  sel_locations_distance_to_line1 <- rgeos::gDistance(sel_locations, sel_locations_distance_line1, byid = TRUE)

  # get all possible pairs of points
  points_combinations <- utils::combn(seq_along(sel_locations_distance_to_line1), 2)

  # determine for each point on which side of the line they are
  sel_locations_distance_line1_coords <- as.data.frame(sp::coordinates(sel_locations_distance_line1)[[1]][[1]])
  sel_locations_coordinates_predicted <- as.data.frame(sp::coordinates(sel_locations))
  if(is.character(all.equal(sel_locations_distance_line1_coords$longitude[1], sel_locations_distance_line1_coords$longitude[2]))) {
    m <- stats::lm(latitude~longitude, data = sel_locations_distance_line1_coords)
    sel_locations_y_predicted <- stats::predict(m, newdata = sel_locations_coordinates_predicted)
    sel_locations_site <- ifelse(sel_locations_y_predicted > sel_locations_coordinates_predicted$latitude, "below", "above")
  } else {
    sel_locations_site <- ifelse(sel_locations_coordinates_predicted$longitude < sel_locations_distance_line1_coords$longitude[1], "left", "right")
  }

  # keep only combinations for which both points are on distinct sites of the line
  index <-
    apply(points_combinations, 2, function(x){
      sel_locations_site[x[1]] != sel_locations_site[x[2]]
    })
  points_combinations <- points_combinations[,index, drop = FALSE]

  # for each pair of points: sum the distances
  if(length(points_combinations) == 0){
    sel_locations_distance_to_line_max <- max(sel_locations_distance_to_line1)
  } else {
    sel_locations_distance_to_line_max <- max(apply(points_combinations, 2, function(x){
      sum(sel_locations_distance_to_line1[x])
    }))
  }

  # compute the ratio
  sel_locations_distance_max/sel_locations_distance_to_line_max
}
