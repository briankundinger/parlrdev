BKSimple <- function(comparisons, m_prior, u_prior,
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
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  indicators <- comparisons[[1]]

  parameter.indicators <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  candidates <- 1:n1
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(indicators)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(indicators)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- rep(0, n1*n2)
  Z <- rep(n1+1, n2)
  L <- 0

  # Gibbs
  for(s in 1:S){
    aZ <- apply(indicators, 2, function(x){
      sum(x == 1 & Z.temp == 1)
    })
    bZ <- apply(indicators, 2, function(x){
      sum(x == 1 & Z.temp == 0)
    })

    m.post <- m_prior + aZ
    u.post <- u_prior + bZ

    m.post <- split(m.post, parameter.indicators)
    m <- as.vector(unlist(sapply(m.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    u.post <- split(u.post, parameter.indicators)
    u <- as.vector(unlist(sapply(u.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    weights <- exp(rowSums(sweep(indicators, 2, log(m), "*"), na.rm = TRUE)
                   - rowSums(sweep(indicators, 2, log(u), "*"), na.rm = TRUE))

    weights <- split(weights, ids[,2])
    offset <- n1*(n2 - L + beta)/(L+ alpha)

    Z <- unname(sapply(weights, function(x){
      sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    }))

    Z.temp <- sapply(Z, function(x){
      if(x < n1 + 1){
        vec <- rep(0, n1)
        vec[x] <- 1
        vec
      }else{
        rep(0, n1)
      }
    })
    L <- sum(Z < n1 + 1)

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


Sadinle17 <- function(comparisons, m_prior, uprior,
                     alpha, beta, S, burn, show_progress = F){
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

  fields <- length(comparisons[[4]])
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  indicators <- comparisons[[1]]

  parameter.indicators <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  candidates <- 1:n1
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(indicators)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(indicators)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- rep(0, n1*n2)
  Z <- rep(n1+1, n2)
  L <- 0



  # Gibbs
  for(s in 1:S){
    aZ <- apply(indicators, 2, function(x){
      sum(x == 1 & Z.temp == 1)
    })
    bZ <- apply(indicators, 2, function(x){
      sum(x == 1 & Z.temp == 0)
    })

    m.post <- m_prior + aZ
    u.post <- u_prior + bZ

    m.post <- split(m.post, parameter.indicators)
    m <- as.vector(unlist(sapply(m.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    u.post <- split(u.post, parameter.indicators)
    u <- as.vector(unlist(sapply(u.post, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    })))

    weights <- exp(rowSums(sweep(indicators, 2, log(m), "*"), na.rm = TRUE)
                   - rowSums(sweep(indicators, 2, log(u), "*"), na.rm = TRUE))

    weights <- split(weights, ids[,2])
    offset <- n1*(n2 - L + beta)/(L+ alpha)

    for(i in 1:n2){
      Z[i] <- n1 + 1
      L <- sum(Z< n1 + 1)
      offset <- (n1 - L)*(n2 - L -1 + beta)/(L+ alpha)
      sampleprob <- weights[[i]]
      sampleprob <- c(sampleprob[!(candidates %in% Z)], offset)
      samplelabel <- c(candidates[!(candidates %in% Z)], n1+1)
      Z[i] <- sample(samplelabel, 1, prob = sampleprob)
    }

    Z.temp <- sapply(Z, function(x){
      if(x < n1 + 1){
        vec <- rep(0, n1)
        vec[x] <- 1
        vec
      }else{
        rep(0, n1)
      }
    })
    L <- sum(Z < n1 + 1)

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
