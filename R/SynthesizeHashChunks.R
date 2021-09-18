SythensizeHash <- function(patterns_hashed){
  counts_big <- Reduce(`+`, map(patterns_hashed, ~.x[[2]]))

  counts_per_rec_big <- patterns_hashed %>%
    map(`[[`, 3) %>%
    flatten()

  hash_list_big <- patterns_hashed %>%
    map(`[[`, 4) %>%
    flatten()

  comparisons <- list(indicators = NULL,
                      n1 = dim(big_file2)[1],
                      n2 = dim(big_file2)[1],
                      nDisagLevs = c(4, 4, 2, 2),
                      Ztrue = NULL)

  patterns <- list(patterns_hashed[[1]][[1]],
                   counts_big,
                   counts_per_rec_big,
                   hash_list_big)

  cd <- list(comparisons, patterns)
}
