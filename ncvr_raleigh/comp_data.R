library(RecordLinkage)
library(dplyr)
library(stringr)

#setwd("ncvr_raleigh")
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
train_set <- 1500
dfA <- readRDS("data/raleigh_A")
dfB <- readRDS("data/raleigh_B")
nA <- nrow(dfA)
nB <- nrow(dfB)

testA <- dfA[-c(1:train_set), ]
testB <- dfB[-c(1:train_set), ]

chunks <- 50
chunk_size <- ceiling(dim(testB)[1]/chunks)
chunk_id <- rep(1:chunks, each = chunk_size)[1:dim(testB)[1]]

chunk_marker <- chunk_id == taskID
chunkB <- testB[chunk_marker, ]
linkage_vars <- c(4, 5, 6, 7, 9, 13)

cd <- compare.linkage(testA[, linkage_vars], chunkB[, linkage_vars],
                      identity1 = testA$voter_id, identity2 = chunkB$voter_id)
saveRDS(cd, file = paste0("comp_vectors/", "cd_", taskID))

