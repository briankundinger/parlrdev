library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)
library(tidyverse)


dfA <- readRDS("data/raleigh_A")
dfB <- readRDS("data/raleigh_B")
nA <- nrow(dfA)
nB <- nrow(dfB)

Ztrue <- MakeZtrue(dfA$voter_id, dfB$voter_id,
                   nA, nB)

comparisons <- readRDS("full_hash/full_hash")
chain <- BKSimple_hash2_big(comparisons)
Zchain <- chain$Z
Zhat <- LinkRecordsBK(Zchain, nA, 1, 1, 2, Inf)
eval <- GetEvaluations(Zhat[[1]], Ztrue, nA)
Zhat_resolved <- ResolveConflicts(Zhat)
eval_resolved <- GetEvaluations(Zhat_resolved, Ztrue, nA)
results <- rbind(eval, eval_resolved) %>%
  as.data.frame()
row.names(results) <- NULL
results$method <- c("fabl", "fabl_resolved")
saveRDS(results, "eval/fabl/result")
