#' Creates biplots of the loadings of a PCA for different combinations of PCs.
#'
#' \code{mp_pca_biplot_variables} takes a \code{matrix} of loadings of a PCA
#' as returned by \code{\link[vegan:scores]{scores}} and creates a biplot of the loadings for
#' each combination of a set of selected PCs.
#'
#' @param loadings_df A \code{matrix} of loadings of a PCA as returned by
#' \code{\link[vegan:scores]{scores}} with the loading values.
#' @param choices A numeric vector of column indices in \code{loadings_df} with the
#' indices of PCs for which to create plots.
#' @param x_limits A numeric vector with two elements indicating the x axis limits
#' of the plots to create.
#' @param c A numeric value representing a multiplication factor that indicates by which
#' amount the length of an arrow is increased to place the variable label.
#' @return An object of class \code{\link[ggplot2:ggplot]{ggplot2}} with
#' a panel for each combination of input PCs showing the loadings for each variable.
#' @export
mp_pca_biplot_variables <- function(loadings_df,
                                    choices = seq_len(ncol(loadings_df)),
                                    x_limits = c(-2, 1),
                                    c = 0.05) {

  # get all pairs of choices
  choices_paris <- as.data.frame(t(utils::combn(x = choices, m = 2)))

  # reformat the data
  d <- apply(choices_paris, 1, function(x){
    data.frame(pair = paste0("PC", min(x), ", PC", max(x)),
               label = rownames(loadings_df),
               pc_x = min(x),
               pc_y = max(x),
               x = loadings_df[, min(x)],
               y = loadings_df[,max(x)],
               stringsAsFactors = FALSE)
  })
  d <- dplyr::bind_rows(d)
  d$intercept = 0

  # compute positions of labels
  label_positions <- data.frame(a = c/sqrt(d$y^2 + d$x^2) * d$x)
  label_positions$b <- sqrt(d$y^2 + d$x^2)/d$x * (label_positions$a)
  label_positions$b <- ifelse(d$y < 0, -label_positions$b, label_positions$b)

  # plot
  ggplot2::ggplot(d,
                  mapping = ggplot2::aes(x = 0,
                                         xend = .data$x,
                                         y = 0,
                                         yend = .data$y)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = .data$intercept),
                        linetype = 2,
                        colour = "gray") +
    ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = .data$intercept),
                        linetype = 2,
                        colour = "gray") +
    ggplot2::geom_segment(arrow = ggplot2::arrow(length = ggplot2::unit(1/2, "picas")),
                          colour = "gray") +
    ggplot2::facet_grid(.data$pc_y~.data$pc_x) +
    ggplot2::geom_text(mapping = ggplot2::aes(x = label_positions$a +  .data$x,
                                              y = label_positions$b + .data$y,
                                              label = .data$label),
                       size = 2.5,
                       colour = "black") +
    ggplot2::expand_limits(x = x_limits) +
    ggplot2::labs(x = "Loading values", y = "Loading values") +
    ggplot2::coord_fixed()
}
