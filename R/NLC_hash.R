BKSimple_hash <- function(comparisons, m_prior, u_prior,
                      alpha, beta, S, burn, show_progress = T, fast =F){
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

  patterns <- GetUniquePatterns(comparisons, fast)

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  #rec2 <- ids[,2]
  candidates <- 1:n1
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(indicators_raw)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(indicators_raw)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- rep(0, n1*n2)
  Z <- rep(n1+1, n2)
  L <- 0
  #AZ <- BZ <- rep(0, length(parameter_split))
  m <- u <- rep(0, length(parameter_split))

  #indicators <- data.frame(indicators_raw, Z.temp)


  hash_id <- patterns[[1]]
  hash <- data.frame(hash_id)
  #hash_split <- split(hash_id, ids[,2])
  unique_patterns <- patterns[[2]]
  pattern_counts <- patterns[[3]]
  P <- dim(unique_patterns)[1]


  # Gibbs
  for(s in 1:S){

    #map2_df(hash_split, Z, ~ .x[.y])

    hash$Z.temp <- Z.temp
    matches <- hash %>%
      group_by(hash_id) %>%
      summarize(matches = sum(Z.temp)) %>%
      select(matches) %>%
      pull()

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
    weights <- unique_weights[hash_id]
    # hash$weight <- unique_weights[hash_id]
    # Z <- hash %>%
    #   group_by(rec2) %>%
    #   summarize(zj = sample(c(candidates, n1 +1), 1, prob = c(weight, offset))) %>%
    #   pull()


    #weights <- exp(rowSums(ratio * indicators_raw, na.rm = TRUE))

    weights <- split(weights, ids[,2])
    offset <- n1*(n2 - L + beta)/(L+ alpha)

    if(fast){


      parallel::clusterExport(cl, c("n1", "offset", "candidates"))
      Z <- parallel::parSapply(cl, weights, function(x){
        sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
      })

    } else {

    Z <- unname(sapply(weights, function(x){
      sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    }))

    }

    Z.temp <- as.vector(sapply(Z, function(x){
      if(x < n1 + 1){
        vec <- rep(0, n1)
        vec[x] <- 1
        vec
      }else{
        rep(0, n1)
      }
    }))

    L <- sum(Z < n1 + 1)
    hash$Z.temp <- Z.temp

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

  Z.SAMPS
}
