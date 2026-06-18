#' Summary statistics of scores
#'
#' Calculates summary statistics of each scores and the total: mean, median,
#' minimum, maximum
#'
#' @param df A data frame.
#' @param col_scores Character string with text at the start of each column name
#'   identifying the columns with scores.
#'
#' @returns A dataframe with the summary statistics.
#' @export
#'
scores_summary <- function(df, col_scores) {

  scores <- subset(df, select = grep(paste0(col_scores, ".*"), colnames(df), value = TRUE))

  # Total scored points per article
  scores$Total <- rowSums(scores, na.rm = TRUE)

  # Calculate summary statistics

  overview <- data.frame(
    Stage = colnames(scores),
    n = colSums(!is.na(scores)),
    Mean = colMeans(scores, na.rm = TRUE),
    Median = mapply(stats::median, scores, na.rm = TRUE),
    Minimum = mapply(min, scores, na.rm = TRUE),
    Maximum = mapply(max, scores, na.rm = TRUE),
    row.names = NULL
  )

  overview
}

