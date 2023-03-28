
nltcs <- read.csv("../data/proc_nltcs.csv")
true_matches <- 8866
# df82 <- nltcs %>%
#   filter(FILE == 82) %>%
#   select(-FILE, -SEQ) %>%
#   mutate(unique_ID = str_sub(REC, start = 3L),
#          DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
#          DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
#          DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
#          STATE = str_pad(STATE, 2, pad = "0"),
#          REGOFF = str_pad(REGOFF, 2, pad = "0"))
#
#
# df89 <- nltcs %>%
#   filter(FILE == 89) %>%
#   select(-FILE, -SEQ) %>%
#   mutate(unique_ID = str_sub(REC, start = 3L)) %>%
#   mutate(unique_ID = str_sub(REC, start = 3L),
#          DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
#          DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
#          DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
#          STATE = str_pad(STATE, 2, pad = "0"),
#          REGOFF = str_pad(REGOFF, 2, pad = "0"))
#
# df82 <- df82[-c(1:2000), ]
# df89 <- df89[-c(1:2000), ]
# sum(df82$unique_ID %in% df89$unique_ID)


# nA <- nrow(df82)
# nB <- nrow(df89)
# Ztrue <- sapply(df89$unique_ID, function(x){
#   match <- which(df82$unique_ID == x)
#   if (length(match) == 0){
#     match <- nA + 1
#   }
#   match
# }) %>%
#   unname()

#sum(Ztrue < nA +1)

evals <- list.files("eval", full.names = T)
eval_df <- lapply(evals, readRDS) %>%
  do.call(rbind, .)

eval_df

eval_df %>%
  group_by(method) %>%
  summarize(total_declared = sum(total_declared),
            true_declared = sum(true_declared),
            false_declared = sum(false_declared)) %>%
  mutate(recall = true_declared / true_matches,
         precision = true_declared / total_declared,
         fmeasure = 2 * (recall  * precision) / (recall + precision)) %>%
  select(recall, precision, fmeasure)

fl_out <- fastLink(df82[, -c(7, 8)], df89[, -c(7, 8)],
                   varnames = names(df82[, -c(7, 8)]),
                   dedupe.matches = T)

sum(df82$unique_ID[fl_out$matches$inds.a] == df89$unique_ID[fl_out$matches$inds.b])
