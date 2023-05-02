
#setwd("../cluster_jobs")
library(dplyr)
library(fastLink)
library(parlrdev)
library(purrr)
library(stringr)

#setwd("../cluster_jobs")
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

Ztrue <- MakeZtrue(df82$unique_ID, df89$unique_ID,
                   nA, nB)
true_matches <- sum(Ztrue < nA + 1)


fl_dedup <- fastLink(df82[, -c(7, 8)], df89[, -c(7, 8)],
                     varnames = names(df82[, -c(7, 8)]),
                     dedupe.matches = T,
                     threshold.match = .5)

Zhat <- MakeZhat_from_fastlink(fl_dedup$matches$inds.a,
                               fl_dedup$matches$inds.b)

declared_correct <- sum(Zhat == Ztrue & Zhat < (nA + 1))
eval <- c(GetEvaluations(Zhat, Ztrue, nA), 'fastlink_resolved')
names(eval)[4] <- "method"

# Full
fl_full <- fastLink(df82[, -c(7, 8)], df89[, -c(7, 8)],
                     varnames = names(df82[, -c(7, 8)]),
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
#setwd("cluster_jobs")
