# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Import data
df <- read.delim("C:/Users/Thomas Rose/Desktop/20261605_reprod_astr_full.txt", header = TRUE) %>%
  select(starts_with("Stage"))


# Pivot data in to long format and convert numbers to discrete values for plotting
final_df <- df %>%
  pivot_longer(
    cols = everything(),
    names_to = "Stages",
    values_to = "Score"
  ) %>%
  mutate(
    Stages = as.numeric(sub("Stage_", "", .$Stages)),
    Stages = as.factor(Stages),
    Score = as.factor(Score)
    )

# Plot bar chart; stacked and recalculated to 100%
ggplot(final_df, aes(fill = Score, x = Stages)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(name = "Percent", expand = c(0, 0), n.breaks = 10, labels = seq(0, 100, 10)) +
  scale_fill_discrete(palette = c("deepskyblue2", "cornflowerblue", "darkblue"), guide = guide_legend(reverse = TRUE)) +
  theme_minimal()
