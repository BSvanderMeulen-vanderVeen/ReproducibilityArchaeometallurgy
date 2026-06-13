
#' Barchart from scores
#'
#' Creates a barchart based on the scores in the reproducibility index.
#'
#' @param df A data frame.
#'
#' @returns The bar chart as ggplot2 object.
#' @export
#'
scores_plot <- function(df) {

  # Pivot data in to long format and convert numbers to discrete values for plotting
  final_df <- df |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "Stages",
      values_to = "Score"
    ) |>
    dplyr::mutate(
      Stages = as.numeric(sub("Stage_", "", .data$Stages)),
      Stages = as.factor(.data$Stages),
      Score = as.factor(.data$Score)
    )

  # Plot bar chart; stacked and recalculated to 100%
  ggplot2::ggplot(final_df, ggplot2::aes(fill = .data$Score, x = .data$Stages)) +
    ggplot2::geom_bar(position = ggplot2::position_fill(reverse = TRUE)) +
    ggplot2::scale_y_continuous(name = "Percent", expand = c(0, 0), n.breaks = 10, labels = seq(0, 100, 10)) +
    ggplot2::scale_fill_discrete(palette = c("deepskyblue2", "cornflowerblue", "darkblue"), guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::theme_minimal()

}
