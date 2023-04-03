library(RecordLinkage)
library(dplyr)
library(stringr)

result_paths <- c("eval/fabl/independent",
  "eval/supervised/aggregate/result",
  "eval/fastlink/result")

results <- lapply(result_paths, readRDS) %>%
  do.call(rbind, .)

results <- results[-2, ]
results[1, 4] <- "fabl"

saveRDS(results, "eval/all_results")
