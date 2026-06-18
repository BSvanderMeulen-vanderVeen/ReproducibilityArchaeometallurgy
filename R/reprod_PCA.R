#' Plotting results of various principal component analyses
#'
#' The function creates plots for individuals and variables of principal
#' component analyses on Centered Log-Ratio transformed data.
#'
#' PCAs are computed with [prcomp()] for every combination of (un)centred and
#' (un)scaled data and with [princomp()] for centred and scaled data as well as
#' uncentred and unscaled data.
#'
#' @param df A data frame.
#' @param groups Column name in `df` according to which data should be grouped
#'   by colour.
#' @param palette_colour Character vector with the colours for the different
#'   `groups`.
#'
#' @returns A list with the plots of the PCA results. Names of the list elements
#'   with the plots follow the notation:
#' * c: centered
#' * s: scaled
#' * u: uncentred or unscaled
#' * I: Individuals
#' * V: Variables
#' * 2: calculation with [princomp()]
#' * no number: calculation with [prcomp()])
#'
#' @export
#'
reprod_PCA <- function(df, groups = "Site", palette_colour = c("red", "orange", "darkblue", "purple", "forestgreen")) {

  # common plot design
  plot_design <- list(
    ggplot2::scale_color_discrete(name = NULL, palette = palette_colour),
    ggplot2::scale_shape_manual(name = NULL, values = rep(16, 5)),
    ggplot2::guides(shape = "none", color = ggplot2::guide_legend(nrow=2, byrow = TRUE)),
    ggplot2::theme_bw(),
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(7, 3, 0, 3, "mm"),
      aspect.ratio = 1,
      legend.position = "bottom",
      legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 1, 0, 0, "mm")),
      legend.key.spacing.y = ggplot2::unit(-5, "pt"),
    )
  )

  # CLR transformation
  clr_df <- as.data.frame(compositions::clr(df[2:14]))

  groups <- as.factor(df[[groups]])

  # Perform PCA with scaled data using prcomp()
  pca_result <- stats::prcomp(clr_df, scale. = TRUE)
  csI <- factoextra::fviz_pca_ind(pca_result, col.ind = groups, geom = "point", title = "Individuals") + plot_design
  csV <- factoextra::fviz_pca_var(pca_result, repel = TRUE, title = "Variables") + plot_design

  # Perform PCA with unscaled data using prcomp()
  pca_result2 <- stats::prcomp(clr_df, scale. = FALSE)
  cuI <- factoextra::fviz_pca_ind(pca_result2, col.ind = groups, geom = "point", title = "Individuals") + plot_design
  cuV <- factoextra::fviz_pca_var(pca_result2, repel = TRUE, title = "Variables") + plot_design

  # Perform PCA with uncentred data, but scaled using prcomp()
  pca_result3 <- stats::prcomp(clr_df, scale. = TRUE, center = FALSE)
  usI <- factoextra::fviz_pca_ind(pca_result3, col.ind = groups, geom = "point", title = "Individuals") + plot_design
  usV <- factoextra::fviz_pca_var(pca_result3, repel = TRUE, title = "Variables") + plot_design

  # Perform PCA with uncentred, unscaled data using prcomp()
  pca_result4 <- stats::prcomp(clr_df, scale. = FALSE, center = FALSE)
  uuI <- factoextra::fviz_pca_ind(pca_result4, col.ind = groups, geom = "point", title = "Individuals") + plot_design
  uuV <- factoextra::fviz_pca_var(pca_result4, repel = TRUE, title = "Variables") + plot_design

  # Perform PCA with scaled and centered data using princomp()
  pca_result5 <- stats::princomp(clr_df, cor = TRUE)
  csI2 <- factoextra::fviz_pca_ind(pca_result5, col.ind = groups, geom = "point", title = "Individuals") + plot_design
  csV2 <- factoextra::fviz_pca_var(pca_result5, repel = TRUE, title = "Variables") + plot_design

  # Perform PCA with unscaled and uncentred data using princomp()
  pca_result6 <- stats::princomp(clr_df)
  cuI2 <- factoextra::fviz_pca_ind(pca_result6, col.ind = groups, geom = "point", title = "Individuals") + plot_design
  cuV2 <- factoextra::fviz_pca_var(pca_result6, repel = TRUE, title = "Variables") + plot_design

  return(
    list(
      "csI" = csI, "csV" = csV, "cuI" = cuI, "cuV" = cuV, "usI" = usI, "usV" = usV, "uuI" = uuI, "uuV" = uuV,
      "csI2" = csI2, "csV2" = csV2, "cuI2" = cuI2, "cuV2" = cuV2
    )
  )
}
