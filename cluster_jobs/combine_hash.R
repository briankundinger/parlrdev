library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)

setwd("cluster_jobs")
hash_paths <- list.files("hash", full.names = T)
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
                    nDisagLevs = c(2, 2, 2, 2, 3),
                    Ztrue = Ztrue)

patterns <- list(patterns_hashed[[1]][[1]],
                 counts_big,
                 counts_per_rec_big,
                 hash_list_big)

comparisons <- list(comparisons, patterns)
saveRDS(cd, file = paste0("full_hash/", "full_hash"))

sapply(patterns_hashed, function(x){
  x[[2]] %>% length()
})
