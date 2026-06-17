#' Reproduce scatterplot figure 4a in article 96
#'
#' This function reproduces figure 4a in article 96 using the data provided in
#' the text and the appendix.
#'
#' @param df_text Data frame with the data provided in the text.
#' @param df_app Data frame with the data provided in the appendix.
#'
#' @returns A list with the reproduced figures
#' @export
#'
#' @importFrom rlang .data
#'
reprod_scatter <- function(df_text, df_app) {

  # define common design elements
  plot_design <- list(
    ggplot2::scale_color_discrete(palette = c("black", "red", "blue", "forestgreen")),
    ggplot2::scale_shape_manual(values = c("Group 1" = 12, "Group 2" = 10, "Group 3" = 17, "Group 4" = 15)),
    ggplot2::scale_x_continuous(name = expression(""^{206}*"Pb/"^{204}*Pb), limits = c(17.3, 18.6), expand = ggplot2::expansion(0)),
    ggplot2::scale_y_continuous(name = expression(""^{207}*"Pb/"^{204}*Pb), limits = c(15.4, 15.7001), expand = ggplot2::expansion(0)),
    ggplot2::theme_bw(base_size = 10),
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(colour = "black", linewidth = 0.45, fill = "red"),
      legend.position = "inside",
      legend.position.inside = c(0, 1),
      legend.justification = c(0, 1),
      legend.title = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(0, 0, 0, 0, "mm"),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0, "mm"),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 3, 0, 0, "mm")),
      legend.key.spacing.y = ggplot2::unit(-5, "pt"),
      plot.margin = ggplot2::margin(0, 5, 0, 0, "mm"),
      aspect.ratio = 746/1040
    )
  )

  # reproduce figure with data from table in text, resulting in removal of outlier
  plot_text <- ggplot2::ggplot(df_text, ggplot2::aes(x = .data$X206Pb.204Pb, y = .data$X207Pb.204Pb, colour = .data$Period, shape = .data$Period)) +
    ggplot2::geom_point() +
    plot_design

  # reproduce figure with data from appendix + Group 4 from data in text
  # (because there is no group 4 in appendix data), resulting in removal of outlier
  plot_app <- ggplot2::ggplot(df_app, ggplot2::aes(x = .data$X206Pb.204Pb, y = .data$X207Pb.204Pb, colour = .data$Period, shape = .data$Period)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = subset(df_text, Period == "Group 4")) +
    plot_design

  return(list("plot_text" = plot_text, "plot_app" = plot_app))

}
