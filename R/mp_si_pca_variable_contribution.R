#' Computes the variable contribution for a PCA
#'
#' \code{mp_si_pca_variable_contribution} takes a \code{matrix} of loadings of a PCA
#' as returned by \code{\link[vegan:scores]{scores}} and computes the contribution of the
#' individual variables used in the PCA.
#'
#' @param loadings A \code{matrix} of loadings of a PCA as returned by
#' \code{\link[vegan:scores]{scores}} with the loading values.
#' @return A \code{data.frame} with one row for each variable used in the PCA
#' and three columns:
#' \describe{
#'   \item{variable_name}{A character vector with the variable names.}
#'   \item{PC}{The principal component.}
#'   \item{contribution}{A numeric vector with the contribution of the variables.}
#' }
#' @export
mp_si_pca_variable_contribution <- function(loadings) {

  # compute variable contributions
  d <- as.data.frame(apply(loadings, 2, function(x) x^2/sum(x^2)))

  # reformatting
  d$variable_name <- rownames(d)
  d <- tidyr::gather(d, key = "PC",
                     value = "contribution",
                     -c("variable_name"))
  d$variable_name <- factor(d$variable_name, levels = rev(unique(d$variable_name)))
  d

}
