FellegiSunter <- function(comparisons, S = 1000, show_progress = T, R = NULL,
                           all_patterns = TRUE, threshold = .75){
  # Implements bipartite record linkage with BK Sampling Mechanism
  #
  # Arguments
  # comparisons = list calculated from from BRL::compareRecords
  # m.prior = prior distribution for m parameters
  # u.prior= prior distribution for u parameters
  # alpha = first parameter of prior for linkage probability
  # beta = second parameter of prior for linkage probability
  # S = number of Gibbs iterations
  # burn = number of iterations to be discarded as burn-in
  # show_progress = set to false to show simulation progress

  fields <- length(comparisons[[4]])
  levels <- comparisons[[4]]
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  indicators_raw <-comparisons[[1]]

  ptm <- proc.time()
  patterns <- GetUniquePatterns2(comparisons, fast, R, all_patterns)
  elapsed_hash <- proc.time() - ptm

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  #rec2 <- ids[,2]

  unique_patterns <- patterns[[1]]
  pattern_counts <- patterns[[2]]
  P <- dim(unique_patterns)[1]
  counts_by_rec <- patterns[[3]]
  hash_to_rec1 <- patterns[[4]]
  # candidates <- 1:n1
  # candidates_P <- 1:(P+1)
  Z.SAMPS <- matrix(NA, nrow = nrow(indicators_raw), ncol = S)
  M.SAMPS <- matrix(NA, nrow = length(parameter_split), ncol = S)
  U.SAMPS <- matrix(NA, nrow = length(parameter_split), ncol = S)
  LAMBDA.SAMPS <- vector(length = S)

  #Initialize
  lambda <-  1 / n1
  m <- sapply(levels, function(x){
    ((x:1)^2)/sum((x:1)^2)
  }) %>%
    unlist() %>%
    as.vector()

  u <- sapply(levels, function(x){
    ((1:x)^2)/sum((1:x)^2)
  }) %>%
    unlist() %>%
    as.vector()

  # Gibbs
  for(s in 1:S){



    match_weight <- sweep(unique_patterns, MARGIN = 2, STAT = log(m), FUN = "*") %>%
      rowSums() %>%
      exp()

    match_weight <- lambda * match_weight


    nonmatch_weight <- sweep(unique_patterns, MARGIN = 2, STAT = log(u), FUN = "*") %>%
      rowSums() %>%
      exp()

    nonmatch_weight <- (1 - lambda) * nonmatch_weight

    prob_match <- match_weight / (match_weight + nonmatch_weight)
    lambda <- sum(prob_match * pattern_counts) / (n1 * n2)

    m <- sweep(unique_patterns, 1, prob_match, "*") %>%
      sweep(., 1, pattern_counts, "*") %>%
      colSums()

    m <- m / sum(prob_match * pattern_counts)

    u <- sweep(unique_patterns, 1,  1 - prob_match, "*") %>%
      sweep(., 1, pattern_counts, "*") %>%
      colSums()

    u <- u / sum((1 - prob_match) * pattern_counts)



    #Z.SAMPS[,s] <- Z
    M.SAMPS[,s] <- m
    U.SAMPS[,s] <- u
    LAMBDA.SAMPS[s] <- lambda

    if(show_progress){
      if (s %% (S / 100) == 0) {
        flush.console()
        cat("\r", paste("Simulation", ": ", s / (S / 100), "% complete", sep = ""))
      }
    }
  }
  #matched_patterns <- which(prob_match > threshold)
  # thing <- lapply(hash_to_rec1, function(x){
  #   lapply(matched_patterns, function(y){
  #     x[[y]]
  #   })
  # })
  hashed_matches <- sapply(hash_to_rec1, function(x){
    thing <- rep(P + 1, n2)
    for(p in 1:P){
      index <- x[[p]]
      thing[index] <- p
    }
    thing
  })

  for(p in 1:P){
    hashed_matches[hashed_matches == p] <- prob_match[p]
  }
  hashed_matches[hashed_matches == P + 1] <- 0
  nonmatch_placeholder <- diag(threshold, n2)
  thing3 <- cbind(t(hashed_matches), nonmatch_placeholder)
  Zhat <- solve_LSAP(thing3, maximum = TRUE) %>%
    as.double()



  list(Zhat = Zhat,
       m = M.SAMPS,
       u = U.SAMPS,
       hash_time = elapsed_hash[3])

}
