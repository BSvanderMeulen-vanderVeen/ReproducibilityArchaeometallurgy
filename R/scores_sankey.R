#' Sankey plot from scores
#'
#' Creates a Sankey plot based on the scores in the reproducibility index.
#'
#' @references
#' Code in the function is based on an example at "[Most basic Sankey Diagram](https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html)" by the R graph gallery.
#' Labels of the stages in the Sankey plot are created using code from CJ Yetman
#' in an answer to "[How to add columnn titles in a Sankey chart
#' networkD3](https://stackoverflow.com/questions/66813278/how-to-add-columnn-titles-in-a-sankey-chart-networkd3)",
#' licensed under [CC BY-SA
#' 4.0](https://creativecommons.org/licenses/by-sa/4.0/).
#'
#' @param df A data frame.
#'
#' @returns The sankey plot list representing a html widget.
#' @export
#'
sankey_plot <- function(df) {

  scores_raw <- subset(df, select = grepl("ID|Stage.*", names(df)))

  # rename stages and include  leading 0 to ensure correct order
  colnames(scores_raw) <- sub("Stage_", "", colnames(scores_raw))
  colnames(scores_raw) <- ifelse(colnames(scores_raw) %in% 0:9, paste0("0", colnames(scores_raw)), colnames(scores_raw))

  scores <- scores_raw |>
    dplyr::mutate(dplyr::across(-.data$ID, ~ paste0("stage", cur_column(), "/score ", .x))) |>
    tidyr::pivot_longer(cols = -.data$ID, names_to = "stage", values_to = "source") |>
    dplyr::group_by(.data$ID) |>
    dplyr::mutate(target = dplyr::lead(source, order_by = .data$stage)) |>
    dplyr::ungroup()

  links <- scores |>
    dplyr::group_by(.data$stage, .data$source, .data$target) |>
    dplyr::summarise(value = dplyr::n(), .groups = "drop_last") |>
    dplyr::ungroup() |>
    tidyr::drop_na() |>
    dplyr::select(-.data$stage)

  nodes <- data.frame(name = unique(c(as.character(links$source), as.character(links$target)))) |>
    dplyr::arrange(.data$name)

  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  nodes$groups <- as.factor(sub(pattern = "^.*/", "", nodes$name))

  nodes$name <- sub("^.*\\s", "", nodes$name)
  links$target <- sub("^.*\\s", "", links$target)
  links$source <- sub("^.*\\s", "", links$source)

  colour <- 'd3.scaleOrdinal() .domain(["score0", "score1", "score2", "scoreNA"]) .range(["#6495ED"])'

  plot <- networkD3::sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name",
    width = 1000, fontSize = 14, NodeGroup = "name",
    sinksRight = FALSE, iteration = 0,
    colourScale = colour)

  plot <- htmlwidgets::onRender(plot, '
    function(el) {
      var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
      var labels = ["Step 2", "Step 1", "Step 3"];
      cols_x.forEach((d, i) => {
        d3.select(el).select("svg")
          .append("text")
          .attr("x", d)
          .attr("y", 12)
          .text("Stage " + (i + 0))
          .attr("font-family", "Arial");
      })
    }
  ')

  plot
}


