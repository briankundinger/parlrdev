vabl_efficient <- function(hash, threshold = 1e-8, tmax = 200){

  check_every <- 10

  ohe <- hash$ohe # One-hot encodings e(h_p)
  P <- dim(ohe)[1]
  total_counts <- hash$total_counts #N_p
  pattern_counts_by_record <- hash$pattern_counts_by_record #N_p_j
  record_counts_by_pattern <- hash$record_counts_by_pattern
  field_marker <- hash$field_marker
  n1 <- hash$n1
  n2 <- hash$n2

  # Priors
  alpha <- rep(1, length(field_marker))
  Beta <- rep(1, length(field_marker))
  alpha_pi <- 1
  beta_pi <- 1

  # Initialize
  a <- rep(1, length(field_marker))
  b <- rep(1, length(field_marker))
  a_pi <- 1
  b_pi <- 1
  t <- 1
  ratio <- 1
  elbo_seq <- vector()

  while(t <= tmax){

    a_sum <- a %>%
      split(., field_marker) %>%
      sapply(., sum) %>%
      digamma(.) %>%
      .[field_marker]

    a_chunk <- digamma(a) - a_sum

    b_sum <- b %>%
      split(., field_marker) %>%
      sapply(., sum) %>%
      digamma(.) %>%
      .[field_marker]
    b_chunk <- digamma(b) - b_sum

    m_p <- ohe %>%
      sweep(., 2, a_chunk, "*") %>%
      rowSums()

    u_p <- ohe %>%
      sweep(., 2, b_chunk, "*") %>%
      rowSums()

    # w_p
    weights = m_p - u_p

    # phi_single
    phi <- exp(digamma(a_pi) - digamma(n1) + weights)
    single <- exp(digamma(b_pi))

    # Phi_j
    C <- sapply(pattern_counts_by_record, function(x){
      x %*% phi + single
    })

    # S(Phi)
    total_nonmatch <- sum(single/ C)

    # N_p(Psi)
    K <- sapply(1:P, function(p){
      sum(record_counts_by_pattern[[p]]/C)
    })

    AZ <- ohe %>%
      sweep(., 1, phi * K, "*") %>%
      colSums()

    BZ <- ohe %>%
      sweep(., 1, total_counts - (phi * K), "*") %>%
      colSums()

    a <- alpha + AZ
    b <- Beta+ BZ

    a_pi <- alpha_pi + n2 - total_nonmatch
    b_pi <- beta_pi + total_nonmatch

    # ELBO
    elbo_pieces <- vector(length = 6)
    elbo_pieces[1] <- sapply(1:n2, function(j){
      sum(pattern_counts_by_record[[j]] * (phi *(weights - log(phi) + log(C[j]))/ C[j] +
                                             u_p))
    }) %>%
      sum(.)

    elbo_pieces[2] <- -sum(single/C *log(single/C)) + total_nonmatch * log(n1) -log(n1)*n2


    elbo_pieces[3] <- log(beta(a_pi, b_pi)) - log(beta(alpha_pi, beta_pi))


    elbo_pieces[4] <- sapply(list(a, b), function(y){
      split(y, field_marker) %>%
        sapply(., function(x){
          sum(lgamma(x)) - lgamma(sum(x))
        })%>%
        sum(.)
    }) %>%
      sum(.)

    elbo_pieces[5] <- -sapply(list(alpha, Beta), function(y){
      split(y, field_marker) %>%
        sapply(., function(x){
          sum(lgamma(x)) - lgamma(sum(x))
        })%>%
        sum(.)
    }) %>%
      sum(.)

    elbo_pieces[6] <- sum((alpha - a) * a_chunk + (Beta - b) * b_chunk)
    elbo <- sum(elbo_pieces)
    elbo_seq <- c(elbo_seq, elbo)

    if(t %% check_every == 0){
      ratio <- abs((elbo_seq[t] - elbo_seq[t - check_every +1])/
                     elbo_seq[t - check_every +1])
    }
    if(ratio < threshold){
      break
    }

    t <- t + 1
    if(t > tmax){
      print("Max iterations have passed before convergence")
      break
    }

  }

  list(pattern_weights = phi,
       C = C,
       a = a,
       b = b,
       a_pi = a_pi,
       b_pi = b_pi,
       elbo_seq = elbo_seq,
       t = t)

}

# exp_log_ratio <- a_chunk - b_chunk
# weights <- ohe %>%
#   sweep(., 2, exp_log_ratio, "*") %>%
#   rowSums()

#elbo[2] <- single * sum((1/C) * log(C))
#elbo[3] <- total_nonmatch * (log(n1) - log(single)) -log(n1)*n2
# elbo[4] <- (alpha_pi - a_pi + n2 - total_nonmatch) *
#   (digamma(a_pi) - digamma(a_pi + b_pi))
# elbo[5] <- (beta_pi - b_pi + total_nonmatch) *
#   (digamma(b_pi) - digamma(a_pi + b_pi))
