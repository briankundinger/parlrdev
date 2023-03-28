library(italy)

train_set <- 1:1250
id1 <- italy10$id[-train_set]
id2 <- italy08$id[-train_set]

true_matches <- sum(id2 %in% id1)


evals <- list.files("eval_italy", full.names = T)
eval_df <- lapply(evals, readRDS) %>%
  do.call(rbind, .)

eval_df %>%
  group_by(method) %>%
  summarize(total_declared = sum(total_declared),
            true_declared = sum(true_declared),
            false_declared = sum(false_declared)) %>%
  mutate(recall = true_declared / true_matches,
         precision = true_declared / total_declared,
         fmeasure = 2 * (recall  * precision) / (recall + precision)) %>%
  select(recall, precision, fmeasure)
