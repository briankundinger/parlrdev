BKSimple2 <- function(comparisons, m_prior, u_prior,
                     alpha, beta, S, burn, show_progress = T){
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
  levels <-
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  indicators_raw <-comparisons[[1]]

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  candidates <- 1:n1
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(indicators_raw)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(indicators_raw)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- factor(rep(0, n1*n2), c(0,1))
  Z <- rep(n1+1, n2)
  L <- 0
  AZ <- BZ <- rep(0, length(parameter_split))
  m <- u <- rep(0, length(parameter_split))

  indicators <- data.frame(indicators_raw, Z.temp)

  # Gibbs
  for(s in 1:S){
    counts <- indicators %>%
      group_by(Z.temp, .drop = FALSE) %>%
      summarise(across(.cols = contains("X"),
                       .fns = sum))

    AZ<- counts %>%
      filter(Z.temp == 1) %>%
      select(contains("X")) %>%
      unlist(use.names = FALSE)

    BZ <- counts %>%
      filter(Z.temp == 0) %>%
      select(contains("X")) %>%
      unlist(use.names = FALSE)


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
      rep(., n1 * n2) %>%
      matrix(., nrow = n1 *n2, byrow = TRUE)

    weights <- exp(rowSums(ratio * indicators_raw, na.rm = TRUE))

    weights <- split(weights, ids[,2])
    offset <- n1*(n2 - L + beta)/(L+ alpha)

    Z <- unname(sapply(weights, function(x){
      sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    }))

    Z.temp <- factor(as.vector(sapply(Z, function(x){
      if(x < n1 + 1){
        vec <- rep(0, n1)
        vec[x] <- 1
        vec
      }else{
        rep(0, n1)
      }
    })), c(0,1))

    L <- sum(Z < n1 + 1)
    indicators$Z.temp <- Z.temp

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
  Z.SAMPS <- Z.SAMPS[,-(1:burn)]
  #L.SAMPS <- L.SAMPS[-(1:burn)]
  #M.SAMPS <- M.SAMPS[,-(1:burn)]
  #U.SAMPS <- U.SAMPS[,-(1:burn)]

  Z.SAMPS

}
