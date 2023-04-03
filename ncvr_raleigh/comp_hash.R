library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#setwd("ncvr_raleigh")
dfA <- readRDS("data/raleigh_A")
dfB <- readRDS("data/raleigh_B")
nA <- nrow(dfA)
nB <- nrow(dfB)

chunks <- 50
chunk_size <- ceiling(dim(dfB)[1]/chunks)
chunk_id <- rep(1:chunks, each = chunk_size)[1:dim(dfB)[1]]

chunk_marker <- chunk_id == taskID
chunkB <- dfB[chunk_marker, ]

linkage_vars <- c(4, 5, 6, 7, 9, 13)
cd <- CompareRecords_fabl(dfA[, linkage_vars], chunkB[, linkage_vars],
                          as.numeric(seq_along(linkage_vars)),
                          types = rep("bi", length(linkage_vars)))
cd[[1]] <- apply(cd[[1]], 2, as.numeric)
hash <- GetUniquePatterns2(cd, R = 10)
saveRDS(hash, file = paste0("hash/", "hash_",
                            str_pad(taskID, 2, pad = "0")))
