#' Stacks the loadings for a PCA in one column.
#'
#' \code{mp_si_pca_stack_loadings} takes a \code{matrix} of loadings of a PCA
#' as returned by \code{\link[vegan:scores]{scores}} and reformats it such that
#' there is one column with the variable names, one with the PC the values refer
#' to and one with the loading values.
#'
#' @param loadings A \code{matrix} of loadings of a PCA as returned by
#' \code{\link[vegan:scores]{scores}} with the loading values.
#' @return A \code{data.frame} with one row for each variable used in the PCA
#' and three columns:
#' \describe{
#'   \item{variable_name}{A character vector with the variable names.}
#'   \item{PC}{The principal component.}
#'   \item{loading}{A numeric vector with the loadings of the variables.}
#' }
#' @export
mp_si_pca_stack_loadings <- function(loadings) {

  d <- as.data.frame(loadings)
  d$variable_name <- rownames(d)
  d  <-
    tidyr::gather(d, key = "PC",
                  value = "loading",
                  -c("variable_name"))
  d$variable_name <- factor(d$variable_name, levels = rev(unique(d$variable_name)))
  d

}
