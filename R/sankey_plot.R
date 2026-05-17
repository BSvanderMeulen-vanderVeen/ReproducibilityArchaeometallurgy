# Sankey plot prep

library(tidyr)
library(dplyr)
library(networkD3)

# read and filter data
scores_raw <- read.delim(
  "C:/Users/Thomas Rose/Desktop/20261605_reprod_astr_full.txt",
  header = TRUE
  )

# Filter and transform data
scores_raw <- subset(scores_raw, select = grepl("ID|Stage.*", names(scores_raw)))

colnames(scores_raw) <- sub("Stage_", "", colnames(scores_raw))
colnames(scores_raw) <- ifelse(colnames(scores_raw) %in% 0:9, paste0("0", colnames(scores_raw)), colnames(scores_raw))

scores <- scores_raw |>
  mutate(across(-ID, ~ paste0("stage", cur_column(), "/score", .x))) |>
  pivot_longer(cols = -ID, names_to = "stage", values_to = "source") |>
  group_by(ID) |>
  mutate(target = lead(source, order_by = stage)) |>
  ungroup()

links <- scores |>
  group_by(stage, source, target) |>
  summarise(value = n(), .groups = "drop_last") |>
  ungroup() |>
  drop_na() |>
  group_by(source) |>
  arrange(target, .by_group = TRUE) |>
  ungroup() |>
  arrange(source) |>
  select(-stage)

nodes <- data.frame(name = unique(c(as.character(links$source), as.character(links$target))))

links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1

sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name",
  width = 2000,
  fontSize = 12,
  sinksRight = TRUE)
