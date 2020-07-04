#' Computes differences between two angles
#'
#' \code{angdiff} computes the difference between two angles.
#'
#' @param x A numeric value in [0;360] representing an angle.
#' @param y A numeric value in [0;360] representing an angle.
#' @return a numeric value representing the difference angle between \code{x}
#' and \code{y}.
#' @seealso .
#' @examples #
#' @export
angdiff <- function(x,
                    y) {

  phi <- abs(x - y) %% 360
  ifelse(phi > 180, 360 - phi, phi)

}
