#' Creates biplots of the loadings of a PCA for different combinations of PCs.
#'
#' \code{mp_pca_biplot_scores} takes a \code{matrix} of scores of a PCA
#' as returned by \code{\link[vegan:scores]{scores}} and creates a biplot of the scores for
#' each combination of a set of selected PCs.
#'
#' @param scores_df A \code{matrix} of loadings of a PCA as returned by
#' \code{\link[vegan:scores]{scores}} with the score values.
#' @param choices A numeric vector of column indices in \code{loadings_df} with the
#' indices of PCs for which to create plots.
#' @return An object of class \code{\link[ggplot2:ggplot]{ggplot2}} with
#' a panel for each combination of input PCs showing the scores for each sample.
#' @export
mp_pca_biplot_scores <- function(scores_df,
                                 choices = seq_len(ncol(scores_df))) {

  # get all pairs of choices
  choices_paris <- as.data.frame(t(utils::combn(x = choices, m = 2)))

  # reformat the data
  d <- apply(choices_paris, 1, function(x){
    data.frame(pair = paste0("PC", min(x), ", PC", max(x)),
               label = rownames(scores_df),
               pc_x = min(x),
               pc_y = max(x),
               x = scores_df[, min(x)],
               y = scores_df[,max(x)],
               stringsAsFactors = FALSE)
  })
  d <- dplyr::bind_rows(d)
  d$intercept = 0

  # plot
  ggplot2::ggplot(d,
                  mapping = ggplot2::aes(x = .data$x,
                                         y = .data$y)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = .data$intercept),
                        linetype = 2,
                        colour = "gray") +
    ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = .data$intercept),
                        linetype = 2,
                        colour = "gray") +
    ggplot2::geom_point() +
    ggplot2::facet_grid(.data$pc_y~.data$pc_x) +
    ggplot2::labs(x = "Loading values", y = "Loading values") +
    ggplot2::coord_fixed()
}
