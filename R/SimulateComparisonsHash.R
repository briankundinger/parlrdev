SimulateComparisonsHash <- function(m, u, levels, n1, n2, overlap){
  parameter_split <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))

  N <- n1 * n2
  ids <- expand.grid(1:n1, 1:n2)
  hash_id <- integer(N)

  df1matches <- df2matches <- seq_len(overlap)
  #pairs <- cbind(df1matches, df2matches)

  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches

  match_index <- which(ids[,1] == ids[,2])[seq_len(overlap)]

  m.list <- split(m, parameter_split)
  u.list <- split(u, parameter_split)

  possible_patterns <- data.frame(GetPossiblePatternsFS_sep(levels))
  P <- dim(possible_patterns)[1]

  m_probs <- map2(possible_patterns, m.list, ~{
    .y[.x]
  }) %>%
    do.call(cbind, .) %>%
    apply(1, prod)

  u_probs <- map2(possible_patterns, u.list, ~{
    .y[.x]
  }) %>%
    do.call(cbind, .) %>%
    apply(1, prod)

  gamma_match <- sample(1:P, overlap, replace = T, m_probs)
  gamma_nonmatch <- sample(1:P, N - overlap, replace = T, u_probs)

  hash_id[match_index] <- gamma_match
  hash_id[-match_index] <- gamma_nonmatch

  # gamma_nonmatch <- sapply(u.list, function(x){
  #   sample(seq_along(x) - 1, N - overlap, replace = T, x)
  # })
  # indicators[-match_index,] <- gamma_nonmatch
  # Sadinle_indicators <- map2(data.frame(indicators), levels, ~FS_to_Sadinle2(.x, .y)) %>%
  #   do.call(cbind, .)

  list(comparisons = hash_id,
       n1 = n1,
       n2 = n2,
       nDisagLevs = levels,
       Ztrue = Ztrue)
}



