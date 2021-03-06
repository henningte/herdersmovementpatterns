% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp_locations_distance_average.R
\name{mp_locations_distance_average}
\alias{mp_locations_distance_average}
\title{Computes the mean distance between campsite locations of a Track object in the order of movement.}
\usage{
mp_locations_distance_average(t, fun = stats::median)
}
\arguments{
\item{t}{An object of class \code{\link[trajectories:Track-class]{Track}}
with a variable \code{location} and \code{campsite}}

\item{fun}{One of \code{mean} or \code{median)}, depending on which function should be
used in order to compute average values.}
}
\value{
a numeric value representing the number of unique (campsite) locations
in \code{t}.
}
\description{
\code{mp_locations_distance_average} computes the mean distance between
locations of a \code{Track} in the order of movement.
Several options are available.
}
\examples{
#
}
\seealso{
.
}
