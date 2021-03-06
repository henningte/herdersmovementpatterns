#' Data for the paper "Patterns in Mongolian Nomadic Household Movement Derived from GPS Trajectories"
#'
#' A dataset containing various movement summary indicators and information on gaps derived from GPS
#' trajectories of 348 nomadic Mongolian households.
#'
#' @format A data frame with 348 rows (one for each household) and 32 variables:
#' \describe{
#'   \item{household_id}{An integer variable with a unique ID for each household.}
#'   \item{number_stv}{An integer variable with the number of detected short-term visits
#'   for each household. A short-term visit is defined as the existence of fixes on less than
#'   four consecutive days between 00:00 AM and 04:00 AM.}
#'   \item{number_stvl}{An integer variable with the number of locations where at least one
#'   short-term visit was detected.}
#'   \item{number_ostvl}{An integer variable with the number of locations where at least one
#'   short-term visit was detected, but no long-term visit (campsite visit) was detected.}
#'   \item{gap_duration_max}{An integer variable with the duration of the longest gap [d].}
#'   \item{gap_duration_min}{An integer variable with the duration of the shortest gap [d].}
#'   \item{gap_duration_mean}{A numeric variable with the average duration of all gaps [d].}
#'   \item{gap_duration_median}{An integer variable with the median duration of all gaps [d].}
#'   \item{gap_number}{An integer variable with the number of gaps.}
#'   \item{date_first_fix}{A \code{Date} variable with the day where the first GPS fix was recorded.}
#'   \item{date_last_fix}{A \code{Date} variable with the day where the last GPS fix was recorded.}
#'   \item{location_altitude}{A numeric variable with the average altitude [m] above sea level of the
#'   locations of a household. See \code{\link{mp_locations_altitude_average}}.}
#'   \item{location_altitude_min}{A numeric variable with the altitude [m] above sea level of the
#'   location with the lowest altitude. See \code{\link{mp_altitude_range}}.}
#'   \item{location_altitude_max}{A numeric variable with the altitude [m] above sea level of the
#'   location with the highest altitude. See \code{\link{mp_altitude_range}}.}
#'   \item{location_altitude_difference}{A numeric variable with the altitude differece [m] between
#'   the highest and lowest location. See \code{\link{mp_altitude_difference}}.}
#'   \item{total_altitude_distance}{A numeric variable with the sum of the absolute altitude distances
#'   covered between the locations in the order these were visites [m]. See \code{\link{mp_altitude_total_distance}}.}
#'   \item{total_relative_altitude_distance}{total_altitude_distance divided by total_distance. See \code{\link{mp_altitude_total_relative_distance}}.}
#'   \item{location_distance}{A numeric variable representing the median distance between subsequently
#'   visited campsite locations. See \code{\link{mp_locations_distance_average}}.}
#'   \item{total_distance}{A numeric variable with the sum of the absolute horizontal distances covered
#'   between the locations in the order these were visites [m]. See \code{\link{mp_distance_total}}.}
#'   \item{longitude_difference}{A numeric variable with the absolute difference of the longitude values of the most eastern
#'   ans most western location of a household [m]. See \code{\link{mp_longitude_difference}}.}
#'   \item{latitude_difference}{A numeric variable with the absolute difference of the latitude values of the most northern
#'   ans most southern location of a household [m]. See \code{\link{mp_latitude_difference}}.}
#'   \item{longitude_sum}{A numeric variable with the sum of the longitude values of the campsite locations in the order these
#'   were visited (equals the difference in the longitude values between the first and last visited
#'   campsite locations). See \code{\link{mp_longitude_sum}}.}
#'   \item{latitude_sum}{A numeric variable with the sum of the latitude values of the campsite locations in the order these
#'   were visited (equals the difference in the latitude values between the first and last visited
#'   campsite locations). See \code{\link{mp_latitude_sum}}.}
#'   \item{chull_area}{A numeric variable with the area of a convex hull spanned around the locations [m\eqn{2}]. See \code{\link{mp_chull}}.}
#'   \item{chull_perimeter}{A numeric variable with the perimeter of a convex hull spanned around the locations [m]. See \code{\link{mp_chull}}.}
#'   \item{locations_direction}{A numeric variable with the median of the differences in the angles (relative to
#'   a fixed line on a horizontal plane) between the beelines between three subsequently visited campsite locations. The
#'   larger the value the more often does a household make strong changes in the direction between subsequently visited
#'   campsite locations. See \code{\link{mp_locations_direction_average}}.}
#'   \item{locations_direction_sd}{A numeric variable with the standard deviation of the differences in the angles (relative to
#'   a fixed line on a horizontal plane) between the beelines between three subsequently visited campsite locations. The
#'   larger the value the more do directions of the beelines differ between different movements. See \code{\link{mp_locations_direction_sd}}.}
#'   \item{locations_number}{An integer variable with the number of unique campsite locations. See \code{\link{mp_locations_count}}.}
#'   \item{locations_number_repeated_visits}{An integer variable with the number of repeated campsite visits at
#'   locations. See \code{\link{mp_locations_count}}.}
#'   \item{gaps_proportion}{A numeric variable with the number of days considered as gaps divided by the number
#'   of days considered. See \code{\link{mp_gap_proportion}}.}
#'   \item{straightness_index}{A numeric variable with the straightness index of the trajectory. The straightness
#'   index is defined as the ratio of total_distance and the beeline distance between the first and last
#'   campsite location in a trajectory. See \code{\link{mp_straightness_index}}.}
#'   \item{linearity}{A numeric variable with an estimate of the trajectory linearity. See \code{\link{mp_linearity}}.}
#' }
#'
"d"
