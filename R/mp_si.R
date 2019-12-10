#' @importFrom Rdpack reprompt
#' @importFrom trajectories Track
#' @importFrom raster area
#' @importFrom sp SpatialLinesLengths
NULL

#' Computes various summary indicators for a Track object.
#'
#' \code{mp_si} is a wrapper function for functions that takes a \code{Track}
#' object as input and return numeric values/vectors as output that summarise the
#' \code{Track} object. The functions are:
#' \enumerate{
#'   \item \code{\link{mp_locations_altitude_average}}
#'   \item \code{\link{mp_altitude_range}}
#'   \item \code{\link{mp_altitude_difference}}
#'   \item \code{\link{mp_altitude_total_distance}}
#'   \item \code{\link{mp_altitude_total_relative_distance}}
#'   \item \code{\link{mp_locations_distance_average}}
#'   \item \code{\link{mp_distance_total}}
#'   \item \code{\link{mp_longitude_difference}}
#'   \item \code{\link{mp_latitude_difference}}
#'   \item \code{\link{mp_longitude_sum}}
#'   \item \code{\link{mp_latitude_sum}}
#'   \item \code{\link{mp_chull}}
#'   \item \code{\link{mp_locations_direction_average}}
#'   \item \code{\link{mp_locations_direction_sd}}
#'   \item \code{\link{mp_locations_count}} with \code{repeated = FALSE}
#'   \item \code{\link{mp_locations_count}} with \code{repeated = TRUE}
#'   \item \code{\link{mp_gap_proportion}}
#'   \item \code{\link{mp_linearity}}
#' }
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
mp_si <- function(t, fun = stats::median){

  # checks
  if(!(inherits(t, "Track"))) {
    stop("t must be a Track object\n")
  }
  if(!(is.function(fun))) {
    stop("fun must be a function")
  }
  if(!(any(colnames(t@data) == "location") && any(colnames(t@data) == "campsite") && any(colnames(t@data) == "norepeatedcampsitevisits"))) {
    stop("t@data must contain a variable 'altitude', 'location' and 'campsite'\n")
  }

  # compute the summary indicators
  campsite <- TRUE
  location_altitude <- mp_locations_altitude_average(t, fun = fun)
  location_altitude_range <- mp_altitude_range(t = t)
  location_altitude_difference <- mp_altitude_difference(t)
  total_altitude_distance <- mp_altitude_total_distance(t)
  total_relative_altitude_distance <- mp_altitude_total_relative_distance(t)
  location_distance <- mp_locations_distance_average(t, fun = fun)
  total_distance = mp_distance_total(t)
  longitude_difference <- mp_longitude_difference(t)
  latitude_difference <- mp_latitude_difference(t)
  longitude_sum <- mp_longitude_sum(t)
  latitude_sum <- mp_latitude_sum(t)
  t_chull <- mp_chull(t)
  chull_area <- raster::area(t_chull)
  chull_perimeter <- sp::SpatialLinesLengths(methods::as(t_chull, "SpatialLines"), longlat = FALSE) * 1000
  locations_direction <- mp_locations_direction_average(t, fun = fun)
  locations_direction_sd <- mp_locations_direction_sd(t)
  locations_number <- mp_locations_count(t, campsite = campsite, repeated = FALSE)
  locations_number_repeated_visits <- mp_locations_count(t, campsite = campsite, repeated = TRUE)
  gaps_proportion <- mp_gap_proportion(t)
  straightness_index <- mp_straightness_index(t)
  linearity <- mp_linearity(t)

  # collect all variables in a data.frame
  data.frame(
    location_altitude = location_altitude,
    location_altitude_min = min(location_altitude_range),
    location_altitude_max = max(location_altitude_range),
    location_altitude_difference = location_altitude_difference,
    total_altitude_distance = total_altitude_distance,
    total_relative_altitude_distance = total_relative_altitude_distance,
    location_distance = location_distance,
    total_distance =total_distance,
    longitude_difference = longitude_difference,
    latitude_difference = latitude_difference,
    longitude_sum = longitude_sum,
    latitude_sum = latitude_sum,
    chull_area = chull_area,
    chull_perimeter = chull_perimeter,
    locations_direction = locations_direction,
    locations_direction_sd = locations_direction_sd,
    locations_number = locations_number,
    locations_number_repeated_visits = locations_number_repeated_visits,
    gaps_proportion = gaps_proportion,
    straightness_index = straightness_index,
    linearity = linearity,
    stringsAsFactors = FALSE
  )

}
