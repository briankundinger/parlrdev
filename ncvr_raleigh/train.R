library(RecordLinkage)
library(dplyr)
library(stringr)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
train_set <- 1500
methods <- c("svm", "rpart")
method <- methods[taskID]
dfA <- readRDS("data/raleigh_A")
dfB <- readRDS("data/raleigh_B")
# nA <- nrow(dfA)
# nB <- nrow(dfB)

trainA <- dfA[1:train_set, ]
trainB <- dfB[1:train_set, ]

linkage_vars <- c(4, 5, 6, 7, 9, 13)
cd <- compare.linkage(trainA[, linkage_vars], trainB[, linkage_vars],
                      identity1 = trainA$voter_id, identity2 = trainB$voter_id)

model <- trainSupv(cd, method = paste(method))
saveRDS(model, file = paste0("trained_models/", method))

