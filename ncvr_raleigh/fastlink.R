library(dplyr)
library(fastLink)
library(parlrdev)
library(purrr)


dfA <- readRDS("data/raleigh_A")
dfB <- readRDS("data/raleigh_B")
nA <- nrow(dfA)
nB <- nrow(dfB)

Ztrue <- MakeZtrue(dfA$voter_id, dfB$voter_id,
                   nA, nB)
true_matches <- sum(Ztrue < nA + 1)

linkage_vars <- c(4, 5, 6, 7, 9, 13)

fl_dedup <- fastLink(dfA[, linkage_vars], dfB[, linkage_vars],
                    varnames = names(dfB[, linkage_vars]),
                    dedupe.matches = T,
                    threshold.match = .5)

Zhat <- MakeZhat_from_fastlink(fl_dedup$matches$inds.a,
                               fl_dedup$matches$inds.b)

declared_correct <- sum(Zhat == Ztrue & Zhat < (nA + 1))
eval <- c(GetEvaluations(Zhat, Ztrue, nA), 'fastlink_resolved')
names(eval)[4] <- "method"

# Full
fl_full <- fastLink(dfA[, linkage_vars], dfB[, linkage_vars],
                     varnames = names(dfB[, linkage_vars]),
                     dedupe.matches = F,
                     threshold.match = .5)

declared_correct <- imap(Ztrue, ~sum((fl_full$matches$inds.a == .x) &
                           (fl_full$matches$inds.b == .y))) %>%
  unlist() %>%
  sum()

recall <- declared_correct/true_matches

total_declared <- length(fl_full$matches$inds.a)
precision <- declared_correct/total_declared
fmeasure <- 2 * (recall * precision) / (recall + precision)

eval_full <- c(recall, precision, fmeasure, "fastlink")
names(eval_full) <- c("Recall", "Precision", "Fmeasure", "method")

result <- rbind(eval_full, eval)
rownames(result) <- NULL

saveRDS(result, "eval/fastlink/result")
# fl_full <- fastLink(df82[, -c(7, 8)], df89[, -c(7, 8)],
#                     varnames = names(df82[, -c(7, 8)]),
#                     dedupe.matches = F)
# sum(df82$unique_ID[fl_out$matches$inds.a] == df89$unique_ID[fl_out$matches$inds.b])
