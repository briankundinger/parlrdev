# S <- 8000
# k <- 100000
# P <- 700
#
#
# candidates <- probs <- 1:k
# candidates2 <- probs2 <- 1:P
# #future::plan(multisession)
# ptm <- proc.time()
# thing <- map_dbl(seq_len(S), ~sample(candidates1, 1))
# ptm - proc.time()
#
# ptm <- proc.time()
# thing2 <- future_map_dbl(seq_len(S), ~sample(candidates, 1, prob = probs),
#                          .options = furrr_options(seed = TRUE))
# ptm - proc.time()
#
#
