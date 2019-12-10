#' @importFrom Rdpack reprompt
#' @importFrom trajectories Track
#' @importFrom sp SpatialPolygons Polygons Polygon proj4string
#' @importFrom raster crs
#' @importFrom grDevices chull
NULL

#' Extracts the convex hull from a Track object.
#'
#' \code{mp_chull} extracts the convex hull from a
#' \code{\link[trajectories:Track-class]{Track}} object and returns it
#' as \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} object.
#'
#' @param t An object of class \code{\link[trajectories:Track-class]{Track}}.
#' @return An object of class \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}}
#' with coordinates corresponding to coordinates of points of the convex hull.
#' @seealso .
#' @examples #
#' @export
mp_chull <- function(t){

  # checks
  if(!(inherits(t, "Track"))){
    stop("t must be a Track object\n")
  }

  # extract the coordinates
  xcoords <- t@sp@coords

  # compute the convex hull
  index <- grDevices::chull(x = xcoords)

  # extract the corresponding coordinates
  ch <- sp::SpatialPolygons(Srl = list(sp::Polygons(srl = list(sp::Polygon(coords = xcoords[index,])), ID = integer(1))))

  # add the crs information
  raster::crs(ch) <- sp::proj4string(t)
  ch

}
