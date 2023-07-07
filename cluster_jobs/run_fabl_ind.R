library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)
library(tidyverse)

nltcs <- read.csv("data/proc_nltcs.csv")
df82 <- nltcs %>%
  filter(FILE == 82) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = str_sub(REC, start = 3L),
         DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = str_pad(STATE, 2, pad = "0"),
         REGOFF = str_pad(REGOFF, 2, pad = "0"))
df89 <- nltcs %>%
  filter(FILE == 89) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = str_sub(REC, start = 3L)) %>%
  mutate(unique_ID = str_sub(REC, start = 3L),
         DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = str_pad(STATE, 2, pad = "0"),
         REGOFF = str_pad(REGOFF, 2, pad = "0"))

nA <- nrow(df82)
nB <- nrow(df89)

Ztrue <- sapply(df89$unique_ID, function(x){
  match <- which(df82$unique_ID == x)
  if (length(match) == 0){
    match <- nA + 1
  }
  match
}) %>%
  unname()

comparisons <- readRDS("cluster_jobs/full_hash/independent/full_hash")
chain <- BKSimple_hash2_big(comparisons)
Zchain <- chain$Z
Zhat <- LinkRecordsBK(Zchain, nA, 1, 1, 2, Inf)
eval <- GetEvaluations(Zhat[[1]], Ztrue, nA)
Zhat_resolved <- ResolveConflicts(Zhat)
eval_resolved <- GetEvaluations(Zhat_resolved, Ztrue, nA)
results <- rbind(eval, eval_resolved) %>%
  as.data.frame()
row.names(results) <- NULL
results$method <- c("fabl_ind_flds", "fabl_ind_flds_resolved")
saveRDS(results, "eval/fabl/independent")

# thing <- comparisons[[2]][[4]] %>%
#   sapply(., function(x){sapply(x, length)})

sort(thing, decreasing = T)
