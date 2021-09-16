BKSimple_hash2 <- function(comparisons, m_prior = 1, u_prior = 1,
                          alpha = 1, beta = 1, S = 1000, burn = 100,
                          show_progress = T, fast = F, R = NULL,
                          all_patterns = TRUE){
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
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  indicators_raw <-comparisons[[1]]

  if (fast) {
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores)
  }

  ptm <- proc.time()
  patterns <- GetUniquePatterns2(comparisons, fast, R, all_patterns)
  elapsed_hash <- proc.time() - ptm

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  #rec2 <- ids[,2]

  #hash_id <- patterns[[1]]
  #hash <- data.frame(hash_id)
  #hash_split <- split(hash_id, ids[,2])
  unique_patterns <- patterns[[1]]
  pattern_counts <- patterns[[2]]
  P <- dim(unique_patterns)[1]
  counts_by_rec <- patterns[[3]]
  hash_to_rec1 <- patterns[[4]]
  candidates <- 1:n1
  candidates_P <- 1:(P+1)
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = length(parameter_split), ncol = S)
  U.SAMPS <- matrix(NA, nrow = length(parameter_split), ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- rep(0, n1*n2)
  Z <- rep(n1+1, n2)
  L <- 0
  #AZ <- BZ <- rep(0, length(parameter_split))
  m <- u <- rep(0, length(parameter_split))
  matches <- rep(0,P)

  #indicators <- data.frame(indicators_raw, Z.temp)





  # Gibbs
  for(s in 1:S){

    #map2_df(hash_split, Z, ~ .x[.y])

    # hash$Z.temp <- Z.temp
    # matches <- hash %>%
    #   group_by(hash_id) %>%
    #   summarize(matches = sum(Z.temp)) %>%
    #   select(matches) %>%
    #   pull()

    AZ <- sweep(unique_patterns, MARGIN = 1, STAT = matches, FUN = "*") %>%
      colSums() %>%
      unname()

    nonmatches <- pattern_counts - matches

    BZ <- sweep(unique_patterns, MARGIN = 1, STAT = nonmatches, FUN = "*") %>%
      colSums() %>%
      unname()


    m.post <- m_prior + AZ
    u.post <- u_prior + BZ

    m.post <- split(m.post, parameter_split)
    m <- as.vector(unlist(sapply(m.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    u.post <- split(u.post, parameter_split)
    u <- as.vector(unlist(sapply(u.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))


    ratio <- (log(m) - log(u)) %>%
      rep(., P) %>%
      matrix(., nrow = P, byrow = TRUE)

    unique_weights <- exp(rowSums(ratio * unique_patterns, na.rm = TRUE))

    hash_weights <- lapply(counts_by_rec, function(x){
      x * unique_weights
    })
    offset <- n1*(n2 - L + beta)/(L+ alpha)
    Z <- unname(sapply(hash_weights, function(x){
      sample(candidates_P, 1, prob = c(x, offset))
    }))
    L <- sum(Z < P + 1)
    hash_matches <- factor(Z, levels = 1:(P+1))
    df <- data.frame(hash_matches)
    matches <- df %>%
      group_by(hash_matches, .drop = F) %>%
      count() %>%
      pull() %>%
      .[-(P+1)]
    #weights <- unique_weights[hash_id]
    # hash$weight <- unique_weights[hash_id]
    # Z <- hash %>%
    #   group_by(rec2) %>%
    #   summarize(zj = sample(c(candidates, n1 +1), 1, prob = c(weight, offset))) %>%
    #   pull()


    #weights <- exp(rowSums(ratio * indicators_raw, na.rm = TRUE))

    #weights <- split(weights, ids[,2])
    # offset <- n1*(n2 - L + beta)/(L+ alpha)

    # if(fast){
    #
    #
    #   parallel::clusterExport(cl, c("n1", "offset", "candidates"))
    #   Z <- parallel::parSapply(cl, weights, function(x){
    #     sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    #   })
    #
    # } else {
    #
    #   Z <- unname(sapply(weights, function(x){
    #     sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    #   }))
    #
    # }

    # Z.temp <- as.vector(sapply(Z, function(x){
    #   if(x < n1 + 1){
    #     vec <- rep(0, n1)
    #     vec[x] <- 1
    #     vec
    #   }else{
    #     rep(0, n1)
    #   }
    # }))

    # L <- sum(Z < n1 + 1)
    # hash$Z.temp <- Z.temp

    Z.SAMPS[,s] <- Z
    M.SAMPS[,s] <- m
    U.SAMPS[,s] <- u
    L.SAMPS[s] <- L

    if(show_progress){
      if (s %% (S / 100) == 0) {
        flush.console()
        cat("\r", paste("Simulation", ": ", s / (S / 100), "% complete", sep = ""))
      }
    }
  }
  if (fast){
    parallel::stopCluster(cl)
  }
  # Z.SAMPS <- Z.SAMPS[,-(1:burn)]
  # L.SAMPS <- L.SAMPS[-(1:burn)]
  # M.SAMPS <- M.SAMPS[,-(1:burn)]
  # U.SAMPS <- U.SAMPS[,-(1:burn)]

  final_gibbs <- apply(Z.SAMPS, 2, function(z){
    unlist(imap(z, ~sample_with_1(hash_to_rec1[[.y]][[.x]], 1)))
  })

  list(Z = final_gibbs,
       m = M.SAMPS,
       u = U.SAMPS,
       hash_time = elapsed_hash[3])

}

BKSimple_hash2_pat <- function(comparisons, m_prior = 1, u_prior = 1,
                           alpha = 1, beta = 1, S = 1000, burn = 100,
                           show_progress = T, fast = F, R = NULL){
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
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  N <- n1 * n2
  #indicators_raw <-comparisons[[1]]

  if (fast) {
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores)
  }

  patterns <- ProcessHash(comparisons, fast, R)

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  #rec2 <- ids[,2]

  #hash_id <- patterns[[1]]
  #hash <- data.frame(hash_id)
  #hash_split <- split(hash_id, ids[,2])
  unique_patterns <- patterns[[1]]
  pattern_counts <- patterns[[2]]
  P <- dim(unique_patterns)[1]
  counts_by_rec <- patterns[[3]]
  hash_to_rec1 <- patterns[[4]]
  candidates <- 1:n1
  candidates_P <- 1:(P+1)
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(unique_patterns)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(unique_patterns)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- rep(0, n1*n2)
  Z <- rep(n1+1, n2)
  L <- 0
  #AZ <- BZ <- rep(0, length(parameter_split))
  m <- u <- rep(0, length(parameter_split))
  matches <- rep(0,P)

  #indicators <- data.frame(indicators_raw, Z.temp)





  # Gibbs
  for(s in 1:S){

    #map2_df(hash_split, Z, ~ .x[.y])

    # hash$Z.temp <- Z.temp
    # matches <- hash %>%
    #   group_by(hash_id) %>%
    #   summarize(matches = sum(Z.temp)) %>%
    #   select(matches) %>%
    #   pull()

    AZ <- sweep(unique_patterns, MARGIN = 1, STAT = matches, FUN = "*") %>%
      colSums() %>%
      unname()

    nonmatches <- pattern_counts - matches

    BZ <- sweep(unique_patterns, MARGIN = 1, STAT = nonmatches, FUN = "*") %>%
      colSums() %>%
      unname()


    m.post <- m_prior + AZ
    u.post <- u_prior + BZ

    m.post <- split(m.post, parameter_split)
    m <- as.vector(unlist(sapply(m.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    u.post <- split(u.post, parameter_split)
    u <- as.vector(unlist(sapply(u.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))


    ratio <- (log(m) - log(u)) %>%
      rep(., P) %>%
      matrix(., nrow = P, byrow = TRUE)

    unique_weights <- exp(rowSums(ratio * unique_patterns, na.rm = TRUE))

    hash_weights <- lapply(counts_by_rec, function(x){
      x * unique_weights
    })
    offset <- n1*(n2 - L + beta)/(L+ alpha)
    Z <- unname(sapply(hash_weights, function(x){
      sample(candidates_P, 1, prob = c(x, offset))
    }))
    L <- sum(Z < P + 1)
    hash_matches <- factor(Z, levels = 1:(P+1))
    df <- data.frame(hash_matches)
    matches <- df %>%
      group_by(hash_matches, .drop = F) %>%
      count() %>%
      pull() %>%
      .[-(P+1)]
    #weights <- unique_weights[hash_id]
    # hash$weight <- unique_weights[hash_id]
    # Z <- hash %>%
    #   group_by(rec2) %>%
    #   summarize(zj = sample(c(candidates, n1 +1), 1, prob = c(weight, offset))) %>%
    #   pull()


    #weights <- exp(rowSums(ratio * indicators_raw, na.rm = TRUE))

    #weights <- split(weights, ids[,2])
    # offset <- n1*(n2 - L + beta)/(L+ alpha)

    # if(fast){
    #
    #
    #   parallel::clusterExport(cl, c("n1", "offset", "candidates"))
    #   Z <- parallel::parSapply(cl, weights, function(x){
    #     sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    #   })
    #
    # } else {
    #
    #   Z <- unname(sapply(weights, function(x){
    #     sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    #   }))
    #
    # }

    # Z.temp <- as.vector(sapply(Z, function(x){
    #   if(x < n1 + 1){
    #     vec <- rep(0, n1)
    #     vec[x] <- 1
    #     vec
    #   }else{
    #     rep(0, n1)
    #   }
    # }))

    # L <- sum(Z < n1 + 1)
    # hash$Z.temp <- Z.temp

    Z.SAMPS[,s] <- Z
    #M.SAMPS[,s] <- m
    #U.SAMPS[,s] <- u
    L.SAMPS[s] <- L

    if(show_progress){
      if (s %% (S / 100) == 0) {
        flush.console()
        cat("\r", paste("Simulation", ": ", s / (S / 100), "% complete", sep = ""))
      }
    }
  }
  if (fast){
    parallel::stopCluster(cl)
  }
  Z.SAMPS <- Z.SAMPS[,-(1:burn)]
  #L.SAMPS <- L.SAMPS[-(1:burn)]
  #M.SAMPS <- M.SAMPS[,-(1:burn)]
  #U.SAMPS <- U.SAMPS[,-(1:burn)]

  final_gibbs <- apply(Z.SAMPS, 2, function(z){
    unlist(imap(z, ~sample_with_1(hash_to_rec1[[.y]][[.x]], 1)))
  })

  final_gibbs

}

BKSimple_hash2_big <- function(comparisons, m_prior = 1, u_prior = 1,
                               alpha = 1, beta = 1, S = 1000, burn = 100,
                               show_progress = T, fast = F, R = NULL){
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

  fields <- length(comparisons[[1]][[4]])
  n1 <- comparisons[[1]][[2]]; n2 <- comparisons[[1]][[3]]
  N <- n1 * n2
  #indicators_raw <-comparisons[[1]]

  if (fast) {
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores)
  }

  #patterns <- ProcessHash(comparisons, fast, R)

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[1]][[4]][x])
  })))

  #ids <- expand.grid(1:n1, 1:n2)
  #rec2 <- ids[,2]

  #hash_id <- patterns[[1]]
  #hash <- data.frame(hash_id)
  #hash_split <- split(hash_id, ids[,2])
  unique_patterns <- comparisons[[2]][[1]]
  pattern_counts <- comparisons[[2]][[2]]
  P <- dim(unique_patterns)[1]
  counts_by_rec <- comparisons[[2]][[3]]
  hash_to_rec1 <- comparisons[[2]][[4]]
  candidates <- 1:n1
  candidates_P <- 1:(P+1)
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(unique_patterns)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(unique_patterns)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- rep(0, n1*n2)
  Z <- rep(n1+1, n2)
  L <- 0
  #AZ <- BZ <- rep(0, length(parameter_split))
  m <- u <- rep(0, length(parameter_split))
  matches <- rep(0,P)

  #indicators <- data.frame(indicators_raw, Z.temp)





  # Gibbs
  for(s in 1:S){

    #map2_df(hash_split, Z, ~ .x[.y])

    # hash$Z.temp <- Z.temp
    # matches <- hash %>%
    #   group_by(hash_id) %>%
    #   summarize(matches = sum(Z.temp)) %>%
    #   select(matches) %>%
    #   pull()

    AZ <- sweep(unique_patterns, MARGIN = 1, STAT = matches, FUN = "*") %>%
      colSums() %>%
      unname()

    nonmatches <- pattern_counts - matches

    BZ <- sweep(unique_patterns, MARGIN = 1, STAT = nonmatches, FUN = "*") %>%
      colSums() %>%
      unname()


    m.post <- m_prior + AZ
    u.post <- u_prior + BZ

    m.post <- split(m.post, parameter_split)
    m <- as.vector(unlist(sapply(m.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    u.post <- split(u.post, parameter_split)
    u <- as.vector(unlist(sapply(u.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))


    ratio <- (log(m) - log(u)) %>%
      rep(., P) %>%
      matrix(., nrow = P, byrow = TRUE)

    unique_weights <- exp(rowSums(ratio * unique_patterns, na.rm = TRUE))

    hash_weights <- lapply(counts_by_rec, function(x){
      x * unique_weights
    })
    offset <- n1*(n2 - L + beta)/(L+ alpha)
    Z <- unname(sapply(hash_weights, function(x){
      sample(candidates_P, 1, prob = c(x, offset))
    }))
    L <- sum(Z < P + 1)
    hash_matches <- factor(Z, levels = 1:(P+1))
    df <- data.frame(hash_matches)
    matches <- df %>%
      group_by(hash_matches, .drop = F) %>%
      count() %>%
      pull() %>%
      .[-(P+1)]
    #weights <- unique_weights[hash_id]
    # hash$weight <- unique_weights[hash_id]
    # Z <- hash %>%
    #   group_by(rec2) %>%
    #   summarize(zj = sample(c(candidates, n1 +1), 1, prob = c(weight, offset))) %>%
    #   pull()


    #weights <- exp(rowSums(ratio * indicators_raw, na.rm = TRUE))

    #weights <- split(weights, ids[,2])
    # offset <- n1*(n2 - L + beta)/(L+ alpha)

    # if(fast){
    #
    #
    #   parallel::clusterExport(cl, c("n1", "offset", "candidates"))
    #   Z <- parallel::parSapply(cl, weights, function(x){
    #     sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    #   })
    #
    # } else {
    #
    #   Z <- unname(sapply(weights, function(x){
    #     sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    #   }))
    #
    # }

    # Z.temp <- as.vector(sapply(Z, function(x){
    #   if(x < n1 + 1){
    #     vec <- rep(0, n1)
    #     vec[x] <- 1
    #     vec
    #   }else{
    #     rep(0, n1)
    #   }
    # }))

    # L <- sum(Z < n1 + 1)
    # hash$Z.temp <- Z.temp

    Z.SAMPS[,s] <- Z
    M.SAMPS[,s] <- m
    U.SAMPS[,s] <- u

    if(show_progress){
      if (s %% (S / 100) == 0) {
        flush.console()
        cat("\r", paste("Simulation", ": ", s / (S / 100), "% complete", sep = ""))
      }
    }
  }
  if (fast){
    parallel::stopCluster(cl)
  }
  Z.SAMPS <- Z.SAMPS[,-(1:burn)]
  M.SAMPS <- M.SAMPS[,-(1:burn)]
  U.SAMPS <- U.SAMPS[,-(1:burn)]

  final_gibbs <- apply(Z.SAMPS, 2, function(z){
    unlist(imap(z, ~sample_with_1(hash_to_rec1[[.y]][[.x]], 1)))
  })

  list(Z = final_gibbs,
       m = M.SAMPS,
       u = U.SAMPS)

}
