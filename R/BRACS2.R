BRACS_BK2 <- function(comparisons, m_prior, u_prior,
                     alpha, beta, S, burn, show_progress = T){
  # Implements bipartite record linkage with BK Sampling Mechanism
  #
  # Arguments
  # comparisons = list calculated from from BRL::compareRecords
  # m_prior = prior distribution for m parameters
  # u_prior= prior distribution for u parameters
  # alpha = first parameter of prior for linkage probability
  # beta = second parameter of prior for linkage probability
  # S = number of Gibbs iterations
  # burn = number of iterations to be discarded as burn-in

  fields <- length(comparisons[[4]])
  n1 <- comparisons[[2]]; n2 <- comparisons[[3]]
  ids <- expand.grid(1:n1, 1:n2)
  indicators_raw <- comparisons[[1]]

  parameter_split <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  varying_fields <- comparisons[[6]]
  n2_vec <- comparisons[[8]]
  K <- length(n2_vec)

  n2_breaks <- c(0, cumsum(n2_vec))
  n2_indices <- lapply(1:K, function(x){
    (n2_breaks[x]+1):n2_breaks[x+1]
  })

  linkage_cluster <- vector(mode = "integer", length = n1*n2)
  for(k in 1:K){
    to_change <- which(ids[, 2] %in% n2_indices[[k]])
    linkage_cluster[to_change] <- k
  }
  linkage_cluster <- factor(linkage_cluster)
  #indicators <- data.frame(indicators, linkage_cluster)

  candidates <- 1:n1
  Z.SAMPS <- matrix(NA, nrow = n2, ncol = S)
  M.SAMPS <- matrix(NA, nrow = dim(indicators_raw)[2], ncol = S)
  U.SAMPS <- matrix(NA, nrow = dim(indicators_raw)[2], ncol = S)
  L.SAMPS <- vector(length = S)
  Z.temp <- factor(rep(0, n1*n2), c(0, 1))
  Z <- rep(n1+1, n2)
  L <- 0
  AZ <- BZ <- matrix(0, nrow = length(parameter_split), ncol = K)
  m <- u <- matrix(0, nrow = length(parameter_split), ncol = K)
  indicators <- data.frame(indicators_raw, linkage_cluster, Z.temp)

  # Gibbs
  for(s in 1:S){

    # AZ[-varying_fields, ]  <- indicators %>%
    #   select(-varying_fields) %>%
    #   filter(Z.temp == 1) %>%
    #   colSums() %>%
    #   unname()
    #
    #
    # BZ[-varying_fields, ]  <- indicators %>%
    #   select(-varying_fields) %>%
    #   filter(Z.temp == 0) %>%
    #   colSums() %>%
    #   unname()

    counts <- indicators %>%
      select(-varying_fields, Z.temp) %>%
      group_by(Z.temp, .drop = FALSE) %>%
      summarise(across(.cols = contains("X"),
                       .fns = sum))

    AZ[-varying_fields, ] <- counts %>%
      filter(Z.temp == 1) %>%
      select(contains("X")) %>%
      unlist(use.names = FALSE)

    BZ[-varying_fields, ] <- counts %>%
      filter(Z.temp == 0) %>%
      select(contains("X")) %>%
      unlist(use.names = FALSE)


    lc_counts <- indicators %>%
      select(varying_fields, Z.temp, linkage_cluster) %>%
      group_by(linkage_cluster, Z.temp, .drop = FALSE) %>%
      summarise(across(.cols = contains("X"),
                       .fns = sum),
                .groups = "drop")

    AZ[varying_fields, ] <- lc_counts %>%
      filter(Z.temp == 1) %>%
      select(contains("X")) %>%
      as.matrix() %>%
      unname() %>%
      t()

    BZ[varying_fields, ] <- lc_counts %>%
      filter(Z.temp == 0) %>%
      select(contains("X")) %>%
      as.matrix() %>%
      unname() %>%
      t()

    # AZ[varying_fields, ] <-
    #   indicators %>%
    #   select(varying_fields, linkage_cluster) %>%
    #   filter(Z.temp == 1) %>%
    #   split(x= ., .$linkage_cluster)%>%
    #   map(colSums)%>%
    #   map(~.x[-length(.x)])%>%
    #   do.call(rbind, .)

    # thing <- indicators %>%
    #   select(varying_fields, linkage_cluster) %>%
    #   filter(Z.temp == 1) %>%
    #   nest_by(linkage_cluster)
    #
    # unlist(lapply(thing$data, function(x){
    #   counts <- colSums(x)
    #   if(is.null(counts)) {
    #     counts <- 0
    #   }
    # }))

    # AZ[-varying_fields, ] <- t(apply(indicators[, -varying_fields], 2, function(x){
    #   sum(x == 1 & Z.temp == 1)
    # }))
    # BZ[-varying_fields, ] <- t(apply(indicators[, -varying_fields], 2, function(x){
    #   sum(x == 1 & Z.temp == 0)
    # }))

    # for(k in 1:K){
    #   AZ[varying_fields, k] <- apply(indicators[, varying_fields], 2, function(x){
    #     sum(x == 1 & Z.temp == 1 & linkage_cluster == k)
    #   })
    #   BZ[varying_fields, k] <- apply(indicators[, varying_fields], 2, function(x){
    #     sum(x == 1 & Z.temp == 0 & linkage_cluster == k)
    #   })
    # }

    m.post <- m_prior + AZ
    u.post <- u_prior + BZ

    # Draw m and u parameters

    m_list <- split(m.post[-varying_fields, 1], parameter_split[-varying_fields])
    m[-varying_fields, ] <- unlist(lapply(m_list, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    }))

    u_list <- split(u.post[-varying_fields, 1], parameter_split[-varying_fields])
    u[-varying_fields, ] <- unlist(lapply(u_list, function(x){
      prob <- MCMCpack::rdirichlet(1, x)
      prob/sum(prob)
    }))

    for(k in 1:K){
      m_list_lc <- split(m.post[varying_fields, k], parameter_split[varying_fields])
      m[varying_fields, k] <- unlist(lapply(m_list_lc, function(x){
        prob <- MCMCpack::rdirichlet(1, x)
        prob/sum(prob)
      }))

      u_list_lc <- split(u.post[varying_fields, k], parameter_split[varying_fields])
      u[varying_fields, k] <- unlist(lapply(u_list_lc, function(x){
        prob <- MCMCpack::rdirichlet(1, x)
        prob/sum(prob)
      }))
    }

    logm <- t(log(m))
    logu <- t(log(u))
    ratio <- logm - logu
    ratio <- ratio[linkage_cluster, ]
    #logm <- logm[linkage_cluster, ]
    #logu <- logu[linkage_cluster, ]

    #thing <- Rfast::mat.mult(logm, indicators_raw)
    #weights <- exp((rowSums(logm * indicators_raw) - rowSums(logu * indicators_raw)))
    weights <- exp((rowSums(ratio * indicators_raw)))
    weights <- split(weights, ids[,2])
    offset <- n1*(n2 - L + beta)/(L + alpha)

    Z <- unname(sapply(weights, function(x){
      sample(c(candidates, n1 + 1), 1, prob = c(x, offset))
    }))
    Z.temp <- as.vector(sapply(Z, function(x){
      if(x < n1 + 1){
        vec <- rep(0, n1)
        vec[x] <- 1
        vec
      }else{
        rep(0, n1)
      }
    }))
    indicators$Z.temp <- Z.temp

    L <- sum(Z < n1 + 1)

    Z.SAMPS[,s] <- Z
    #M.SAMPS[,s] <- m
    #U.SAMPS[,s] <- u
    #L.SAMPS[s] <- L

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
