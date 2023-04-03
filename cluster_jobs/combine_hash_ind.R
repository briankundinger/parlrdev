library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)

#setwd("cluster_jobs")
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
hash_paths <- list.files("hash/independent", full.names = T)
patterns_hashed <- lapply(hash_paths, readRDS)

counts_big <- Reduce(`+`, map(patterns_hashed, ~.x[[2]]))

counts_per_rec_big <- patterns_hashed %>%
  map(`[[`, 3) %>%
  flatten()

hash_list_big <- patterns_hashed %>%
  map(`[[`, 4) %>%
  flatten()

cd <- list(indicators = NULL,
                    n1 = dim(df82)[1],
                    n2 = dim(df89)[1],
                    nDisagLevs = c(2, 2, 2, 2, 2, 2),
                    Ztrue = NULL)

patterns <- list(patterns_hashed[[1]][[1]],
                 counts_big,
                 counts_per_rec_big,
                 hash_list_big)

comparisons <- list(cd, patterns)
saveRDS(comparisons, file = paste0("full_hash/independent/", "full_hash"))

