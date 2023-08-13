vabl_base <- function(comparisons, threshold = 1e-5){
  # Implements bipartite record linkage with BK Sampling Mechanism
  #
  # Arguments
  # comparisons = list calculated from from BRL::compareRe,cords
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
  indicators <- data.frame(comparisons[[1]])

  parameter.indicators <- as.vector(unlist(sapply(1:fields, function(x){
    rep(x, comparisons[[4]][x])
  })))

  ids <- expand.grid(1:n1, 1:n2)
  candidates <- 1:n1

  # Storage
  phi_samps <- matrix(NA, nrow = n2, ncol = n_iter)
  a_samps <- matrix(NA, nrow = dim(indicators)[2], ncol = n_iter)
  b_samps <- matrix(NA, nrow = dim(indicators)[2], ncol = n_iter)
  a_pi_samps <- vector(length = n_iter)
  b_pi_samps <- vector(length = n_iter)

  # Priors
  alpha <- rep(1, length(parameter.indicators))
  beta <- rep(1, length(parameter.indicators))
  alpha_pi <- 1
  beta_pi <- 1

  # Initialize
  a <- rep(1, length(parameter.indicators))
  b <- rep(1, length(parameter.indicators))
  a_pi <- 1
  b_pi <- 1

  # Computational savings
  # totals <- indicators %>%
  #   colSums()

for(i in 1:n_iter){
  a_split <- a %>%
    split(., parameter.indicators) %>%
    lapply(., digamma)
  a_sum <- a %>%
    split(., parameter.indicators) %>%
    lapply(., sum) %>%
    lapply(., digamma)
  a_chunk <- map2(a_split, a_sum, ~.x - .y) %>%
    unlist()

  b_split <- b %>%
    split(., parameter.indicators) %>%
    lapply(., digamma)
  b_sum <- b %>%
    split(., parameter.indicators) %>%
    lapply(., sum) %>%
    lapply(., digamma)
  b_chunk <- map2(b_split, b_sum, ~.x - .y) %>%
    unlist()
  exp_log_ratio <- a_chunk - b_chunk

  weight <- indicators %>%
    sweep(., 2, exp_log_ratio, "*") %>%
    rowSums() %>%
    exp()

  nonmatch_weight <- exp(digamma(b_pi) - digamma(a_pi) + digamma(n1))

  phi_ij <- split(weight, ids[, 2]) %>%
    lapply(., function(x){
      x / sum(x, nonmatch_weight)
    }) %>%
    unlist()

  phi_0j <- split(weight, ids[, 2]) %>%
    lapply(., function(x){
      nonmatch_weight / sum(x, nonmatch_weight)
    }) %>%
    unlist()

  a_phi <- indicators %>%
    sweep(., 1, phi_ij, "*") %>%
   colSums()
  a <- alpha + a_phi

  b_phi <- indicators %>%
    sweep(., 1, 1 - phi_ij, "*") %>%
    colSums()
  b <- beta + b_phi
  #b <- totals - a_phi + beta



  a_pi <- alpha_pi + sum(1 - phi_0j)
  b_pi <- beta_pi + sum(phi_0j)
  a_pi_samps[i] <- a_pi
  b_pi_samps[i] <- b_pi
  print(i)
}
  # hist(phi_ij[phi_ij > .5])
  # plot(a_pi_samps)
  # linked <- phi_ij > .5
  # Z_hat <- ids[, 1][linked]
  #
  # Z_hat

  list(phi = phi_ij,
       a = a,
       b = b,
       a_pi = a_pi,
       b_pi = b_pi)
}
