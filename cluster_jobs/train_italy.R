library(RecordLinkage)
library(dplyr)
library(stringr)
library(italy)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

methods <- c("svm", "rpart")
method <- methods[taskID]
train_set <- 1:1250
id1 <- italy10$id[train_set]
id2 <- italy08$id[train_set]

df1 <- italy10 %>%
  select(-id, -PARENT)

df2 <- italy08 %>%
  select(-id, -PARENT)

df1 <- df1[train_set, ]
df2 <- df2[train_set, ]


cd <- compare.linkage(df1, df2,
                      identity1 = id1, identity2 = id2)

model <- trainSupv(cd, method = paste(method))
saveRDS(model, file = paste0("trained_models/", method))
