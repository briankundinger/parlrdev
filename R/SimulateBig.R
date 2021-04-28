SimulateBig <- function(m, u, levels, n1, n2_list, overlap){
  parameter_split <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))
  overlap_per_chunk <- overlap/length(n2_list)
  possible_patterns <- data.frame(GetPossiblePatternsFS_sep(levels))


  patterns_big <- lapply(n2_list, function(z){

  N <- n1 * length(z)
  ids <- expand.grid(1:n1, z)
  hash_id <- integer(N)

  df1matches <- df2matches <- z[1:(length(z)/2)]
  #pairs <- cbind(df1matches, df2matches)

  Ztrue <- rep(n1 + 1, length(z))
  Ztrue[1:(length(z)/2)] <- df1matches

  match_index <- which(ids[,1] == ids[,2])[seq_len(overlap_per_chunk)]

  m.list <- split(m, parameter_split)
  u.list <- split(u, parameter_split)


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

  gamma_match <- sample(1:P, overlap_per_chunk, replace = T, m_probs)
  gamma_nonmatch <- sample(1:P, N - overlap_per_chunk, replace = T, u_probs)

  hash_id[match_index] <- gamma_match
  hash_id[-match_index] <- gamma_nonmatch

  # gamma_nonmatch <- sapply(u.list, function(x){
  #   sample(seq_along(x) - 1, N - overlap, replace = T, x)
  # })
  # indicators[-match_index,] <- gamma_nonmatch
  # Sadinle_indicators <- map2(data.frame(indicators), levels, ~FS_to_Sadinle2(.x, .y)) %>%
  #   do.call(cbind, .)


  cd <- list(comparisons = hash_id,
       n1 = n1,
       n2_index = z,
       nDisagLevs = levels,
       Ztrue = Ztrue)

  patterns_chunk <- ProcessHashBig(cd, fast = F, R = 10, Ztrue)
  patterns_chunk
  })

  counts_big <- Reduce(`+`, map(patterns_big, ~.x[[2]]))
  counts_per_rec_big <- patterns_big %>%
    map(`[[`, 3) %>%
    flatten()

  hash_list_big <- patterns_big %>%
    map(`[[`, 4) %>%
    flatten()


  n2 <- max(do.call(max, n2_list))

  Ztrue <- patterns_big %>%
    map(`[[`, 5) %>%
    do.call(c, .)

  possible_patterns <- GetPossiblePatternsSad_sep(levels)

  comparisons <- list(indicators = NULL,
                      n1 = n1,
                      n2 = n2,
                      nDisagLevs = levels,
                      Ztrue = Ztrue)

  patterns <- list(possible_patterns,
       counts_big,
       counts_per_rec_big,
       hash_list_big)

  list(comparisons, patterns)

}
