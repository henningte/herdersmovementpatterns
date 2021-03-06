% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp_locations_altitude_average.R
\name{mp_locations_altitude_average}
\alias{mp_locations_altitude_average}
\title{Computes the average altitude of locations of a Track object.}
\usage{
mp_locations_altitude_average(t, fun = stats::median, campsite = TRUE)
}
\arguments{
\item{t}{An object of class \code{\link[trajectories:Track-class]{Track}}
with a variable \code{altitude} and a variable \code{location} and \code{campsite}
as created by \code{\link[herdersTA]{locationsTrack}} in the data slot.}

\item{fun}{One of \code{mean} or \code{median)}, depending on which function should be
used in order to compute average values.}

\item{campsite}{A logical value indicating if only campsites should be considered
(\code{TRUE}) or any locations (\code{FALSE}).}
}
\value{
a numeric value representing the average altitude of the locations in \code{t}.
}
\description{
\code{mp_locations_altitude_average} computes the average altitude of lcoations of a
\code{Track}. Several options are available. For each
location, only one value is considered (i.e. thetemporal duration of visits is not
considered as weights during computations).
}
\examples{
#
}
\seealso{
.
}
