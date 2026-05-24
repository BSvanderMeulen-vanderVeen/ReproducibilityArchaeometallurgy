# Import data and remove comments
scores <- readr::read_csv(./reprod_astr_anonymised.csv)

scores <- subset(scores, select = grep("Stage_.*", colnames(scores), value = TRUE))

# Total scored points per article
scores$Total <- rowSums(scores, na.rm = TRUE)

# Add summary statistics

overview <- data.frame(
  Stage = colnames(scores),
  n = colSums(!is.na(scores)),
  Mean = colMeans(scores, na.rm = TRUE),
  Median = mapply(median, scores, na.rm = TRUE),
  Minimum = mapply(min, scores, na.rm = TRUE),
  Maximum = mapply(max, scores, na.rm = TRUE)
  )

# Save data frame as .csv
utils::write.csv2(overview, "table_2.csv", na = "", row.names = FALSE)
