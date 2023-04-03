library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)

dfA <- readRDS("data/raleigh_A")
dfB <- readRDS("data/raleigh_B")
nA <- nrow(dfA)
nB <- nrow(dfB)
hash_paths <- list.files("hash/", full.names = T)
patterns_hashed <- lapply(hash_paths, readRDS)

counts_big <- Reduce(`+`, map(patterns_hashed, ~.x[[2]]))

counts_per_rec_big <- patterns_hashed %>%
  map(`[[`, 3) %>%
  flatten()

hash_list_big <- patterns_hashed %>%
  map(`[[`, 4) %>%
  flatten()

cd <- list(indicators = NULL,
                    n1 = nA,
                    n2 = nB,
                    nDisagLevs =rep(2, 6),
                    Ztrue = NULL)

patterns <- list(patterns_hashed[[1]][[1]],
                 counts_big,
                 counts_per_rec_big,
                 hash_list_big)

comparisons <- list(cd, patterns)
saveRDS(comparisons, file = paste0("full_hash/", "full_hash"))

