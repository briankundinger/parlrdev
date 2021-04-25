SimulateComparisons <- function(m, u, levels, n1, n2, overlap){
  parameter_split <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))

  N <- n1 * n2
  ids <- expand.grid(1:n1, 1:n2)
  indicators <- matrix(NA, nrow = N, ncol = length(parameter_split))

  df1matches <- sample(1:n1, overlap, replace = FALSE)
  df2matches <- sample(1:n2, overlap, replace = FALSE)
  pairs <- cbind(df1matches, df2matches)

  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches

  match_index <- apply(pairs, 1, function(x){
    which(ids[, 1] == x[1] & ids[, 2] == x[2])
  })
  m.list <- split(m, parameter_split)
  u.list <- split(u, parameter_split)

  gamma_match <- lapply(m.list, function(x){
    rmultinom(overlap, 1, x)
  })
  indicators[match_index,] <- t(do.call(rbind, gamma_match))

  gamma_nonmatch <- lapply(u.list, function(x){
    rmultinom(N - overlap, 1, x)
  })
  indicators[-match_index,] <- t(do.call(rbind, gamma_nonmatch))
  list(comparisons = indicators,
       n1 = n1,
       n2 = n2,
       nDisagLevs = levels,
       Ztrue = Ztrue)
}

SimulateComparisonsFast <- function(m, u, levels, n1, n2, overlap){
  parameter_split <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))

  N <- n1 * n2
  ids <- expand.grid(1:n1, 1:n2)
  indicators <- matrix(NA, nrow = N, ncol = length(levels))

  df1matches <- df2matches <- seq_len(overlap)
  #pairs <- cbind(df1matches, df2matches)

  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches

  match_index <- which(ids[,1] == ids[,2])[seq_len(overlap)]

  m.list <- split(m, parameter_split)
  u.list <- split(u, parameter_split)

  gamma_match <- sapply(m.list, function(x){
    sample(seq_along(x) - 1, overlap, replace = T, x)
  })

  indicators[match_index,] <- gamma_match

  gamma_nonmatch <- sapply(u.list, function(x){
    sample(seq_along(x) - 1, N - overlap, replace = T, x)
  })
  indicators[-match_index,] <- gamma_nonmatch
  Sadinle_indicators <- map2(data.frame(indicators), levels, ~FS_to_Sadinle2(.x, .y)) %>%
    do.call(cbind, .)

  list(comparisons = Sadinle_indicators,
       n1 = n1,
       n2 = n2,
       nDisagLevs = levels,
       Ztrue = Ztrue)
}

SimulateComparisonsLC <- function(m, u, levels, varying_fields,
                                   n1, n2, n1_vec, n2_vec, overlap_vec){
  parameter_split <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))

  N <- n1 * n2
  ids <- expand.grid(1:n1, 1:n2)
  indicators <- matrix(NA, nrow = N, ncol = length(parameter_split))

  K <- dim(m)[1]
  N_vec <- n2_vec * n1
  overlap <- sum(overlap_vec)
  n2_breaks <- c(0, cumsum(n2_vec))
  n2_indices <- lapply(1:K, function(x){
    (n2_breaks[x]+1):n2_breaks[x+1]
  })

  linkage_cluster <- vector(mode = "integer", length = N)
  for(k in 1:K){
    to_change <- which(ids[, 2] %in% n2_indices[[k]])
    linkage_cluster[to_change] <- k
  }

  df1matches <- matrix(sample(1:n1, overlap, replace = FALSE), ncol = K)
  df2matches <- mapply(function(x, y){
    sample(x, y, replace = FALSE)
  }, x = n2_indices, y = overlap_vec)

  for(k in 1:K){
    pairs <- cbind(df1matches[, k], df2matches[, k])
    match_index <- apply(pairs, 1, function(x){
      which(ids[, 1] == x[1] & ids[, 2] == x[2])
    })
    m.list <- split(m[k, ], parameter_split)
    u.list <- split(u[k, ], parameter_split)
    gamma_match <- lapply(m.list, function(x){
      rmultinom(overlap_vec[k], 1, x)
    })
    indicators[match_index,] <- t(do.call(rbind, gamma_match))

    gamma_nonmatch <- lapply(u.list, function(x){
      rmultinom(N_vec[k] - overlap_vec[k], 1, x)
    })

    nonmatch_index <- which(!(seq_len(N) %in%  match_index) &
                              linkage_cluster == k )
    indicators[nonmatch_index, ] <- t(do.call(rbind, gamma_nonmatch))
  }
  df1matches <- as.vector(df1matches)
  df2matches <- as.vector(df2matches)


  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches
  list(indicators, n1, n2, levels, Ztrue, varying_fields, n1_vec, n2_vec)
}

SimulateComparisonsLC2<- function(m, u, levels, varying_fields,
                                  n1, n2, n1_vec, n2_vec, overlap_vec){
  parameter_split <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))

  N <- n1 * n2
  ids <- expand.grid(1:n1, 1:n2)
  indicators <- matrix(NA, nrow = N, ncol = length(parameter_split))

  K <- dim(m)[1]
  nonmatches_within_clusters <- n1_vec * n2_vec - overlap_vec[-1]
  nonmatches_across_clusters <- N - sum(n1_vec * n2_vec) - overlap_vec[1]
  nonmatch_vec <- c(nonmatches_across_clusters, nonmatches_within_clusters)
  overlap <- sum(overlap_vec)
  n1_breaks <- c(0, cumsum(n1_vec))
  n1_indices <- lapply(1:(K-1), function(x){
    (n1_breaks[x]+1):n1_breaks[x+1]
  })
  n2_breaks <- c(0, cumsum(n2_vec))
  n2_indices <- lapply(1:(K-1), function(x){
    (n2_breaks[x]+1):n2_breaks[x+1]
  })

  linkage_cluster <- rep(0, N)
  for(k in 1:(K-1)){
    to_change <- which((ids[, 2] %in% n2_indices[[k]] &
                          ids[, 1] %in% n1_indices[[k]]))
    linkage_cluster[to_change] <- k
  }

  df1matches <- mapply(function(x, y){
    sample(x, y, replace = FALSE)
  }, x = n1_indices, y = overlap_vec[-1], SIMPLIFY = FALSE)

  df2matches <- mapply(function(x, y){
    sample(x, y, replace = FALSE)
  }, x = n2_indices, y = overlap_vec[-1], SIMPLIFY = FALSE)

  start <- 2

  ## Across cluster matches
  if(overlap_vec[1] != 0){
  df1_free <- purrr::map2(n1_indices, df1matches,
                   ~ .x[!(.x %in% .y)])

  df2_free <- purrr::map2(n2_indices, df2matches,
                   ~ .x[!(.x %in% .y)])

  crossmatches <- sapply(1:overlap_vec[1], function(x){

  k <- sample(1:(K-1), 1)
  df1 <- sample(df1_free[[k]], 1)
  df2 <- sample(unlist(df2_free[[-k]]), 1)
  c(df1, df2)
  })

  df1matches <- append(list(crossmatches[1,]), df1matches)
  df2matches <- append(list(crossmatches[2,]), df2matches)

  start <- 1
  } else {
    df1matches <- append(list(NULL), df1matches)
    df2matches <- append(list(NULL), df2matches)
  }

  for(k in start:K){
    pairs <- cbind(df1matches[[k]], df2matches[[k]])
    match_index <- apply(pairs, 1, function(x){
      which(ids[, 1] == x[1] & ids[, 2] == x[2])
    })
    m.list <- split(m[k, ], parameter_split)
    u.list <- split(u[k, ], parameter_split)
    gamma_match <- lapply(m.list, function(x){
      rmultinom(overlap_vec[k], 1, x)
    })
    indicators[match_index,] <- t(do.call(rbind, gamma_match))

    gamma_nonmatch <- lapply(u.list, function(x){
      rmultinom(nonmatch_vec[k], 1, x)
    })

    nonmatch_index <- which(!(seq_len(N) %in%  match_index) &
                              linkage_cluster == (k-1) )
    indicators[nonmatch_index, ] <- t(do.call(rbind, gamma_nonmatch))
  }

  if(overlap_vec[1] == 0){
    k <- 1
    u.list <- split(u[k, ], parameter_split)

    gamma_nonmatch <- lapply(u.list, function(x){
      rmultinom(nonmatch_vec[k], 1, x)
    })
    nonmatch_index <- which(linkage_cluster == (k-1) )
    indicators[nonmatch_index, ] <- t(do.call(rbind, gamma_nonmatch))
  }


  df1matches <- as.vector(unlist(df1matches))
  df2matches <- as.vector(unlist(df2matches))


  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches
  list(indicators, n1, n2, levels, Ztrue, varying_fields, n1_vec, n2_vec)
}

# SimulateComparisonsLC2_no_cross <- function(m, u, levels, varying_fields,
#                                    n1, n2, n1_vec, n2_vec, overlap_vec){
#   parameter_split <- unlist(lapply(1:length(levels), function(x){
#     rep(x, levels[x])
#   }))
#
#   N <- n1 * n2
#   ids <- expand.grid(1:n1, 1:n2)
#   indicators <- matrix(NA, nrow = N, ncol = length(parameter_split))
#
#   K <- dim(m)[1]
#   nonmatch_vec <- n1_vec * n2_vec - overlap_vec[-1]
#   overlap <- sum(overlap_vec)
#   n1_breaks <- c(0, cumsum(n1_vec))
#   n1_indices <- lapply(1:(K-1), function(x){
#     (n1_breaks[x]+1):n1_breaks[x+1]
#   })
#   n2_breaks <- c(0, cumsum(n2_vec))
#   n2_indices <- lapply(1:(K-1), function(x){
#     (n2_breaks[x]+1):n2_breaks[x+1]
#   })
#
#   linkage_cluster <- rep(0, N)
#   for(k in 1:(K-1)){
#     to_change <- which((ids[, 2] %in% n2_indices[[k]] &
#                           ids[, 1] %in% n1_indices[[k]]))
#     linkage_cluster[to_change] <- k
#   }
#
#   df1matches <- mapply(function(x, y){
#     sample(x, y, replace = FALSE)
#   }, x = n1_indices, y = overlap_vec[-1], SIMPLIFY = FALSE)
#
#   df2matches <- mapply(function(x, y){
#     sample(x, y, replace = FALSE)
#   }, x = n2_indices, y = overlap_vec[-1], SIMPLIFY = FALSE)
#
#   for(k in 1:K){
#     pairs <- cbind(df1matches[[k]], df2matches[[k]])
#     match_index <- apply(pairs, 1, function(x){
#       which(ids[, 1] == x[1] & ids[, 2] == x[2])
#     })
#     m.list <- split(m[k, ], parameter_split)
#     u.list <- split(u[k, ], parameter_split)
#     gamma_match <- lapply(m.list, function(x){
#       rmultinom(overlap_vec[k], 1, x)
#     })
#     indicators[match_index,] <- t(do.call(rbind, gamma_match))
#
#     gamma_nonmatch <- lapply(u.list, function(x){
#       rmultinom(nonmatch_vec[k], 1, x)
#     })
#
#     nonmatch_index <- which(!(seq_len(N) %in%  match_index) &
#                               linkage_cluster ==  (k-1))
#     indicators[nonmatch_index, ] <- t(do.call(rbind, gamma_nonmatch))
#   }
#
#
#   df1matches <- as.vector(unlist(df1matches))
#   df2matches <- as.vector(unlist(df2matches))
#
#
#   Ztrue <- rep(n1 + 1, n2)
#   Ztrue[df2matches] <- df1matches
#   list(indicators, n1, n2, levels, Ztrue, varying_fields, n1_vec, n2_vec)
# }

# SimulateComparisonsLC2 <- function(m, u, levels, varying.fields,
#                                   n1, n2, n1.vec, n2.vec, overlap.vec){
#   #
#   #
#   #
#   #
#   #
#   N <- n1*n2
#   ids <- expand.grid(1:n1, 1:n2)
#   parameter.indicators <- as.vector(unlist(sapply(1:length(levels), function(x){
#     rep(x, levels[x])
#   })))
#
#   K <- dim(m)[1]
#   N.vec <- n2.vec * n1;
#   overlap <- sum(overlap.vec)
#
#   # n1.breaks <- c(0, cumsum(n1.vec))
#   n2.breaks <- c(0, cumsum(n2.vec))
#   # n1.lc <- lapply(1:K, function(x){
#   #   (n1.breaks[x]+1):n1.breaks[x+1]
#   # })
#   n2.lc <- lapply(1:K, function(x){
#     (n2.breaks[x]+1):n2.breaks[x+1]
#   })
#
#   lc <- as.vector(sapply(1:K, function(x){
#     rep(x, n2.vec[x])
#   }))
#
#   lc.ids <- split(1:n2, lc)
#   lc.info <- NULL
#   for(k in 1:K){
#     lc.info[[k]] <- list(lc.ids[[k]], overlap.vec[k])
#   }
#
#   lc.long <- as.vector(sapply(1:K, function(x){
#     rep(rep(x, n2.vec[x]), n1)
#   }))
#
#   match.vectors <- nonmatch.vectors <- NULL
#   for(k in 1:K){
#     temp.match <- matrix(NA, nrow = overlap.vec[k], ncol = dim(m)[2])
#     temp.nonmatch <- matrix(NA, nrow = N.vec[k] - overlap.vec[k], ncol = dim(m)[2])
#     m.list <- split(m[k, ], parameter.indicators)
#     u.list <- split(u[k, ], parameter.indicators)
#
#
#     for(i in 1:overlap.vec[k]){
#       temp.match[i,]<- unname(unlist(lapply(m.list, function(x){
#         rmultinom(1, 1, x)
#       })))
#     }
#
#     for(i in 1:(N.vec[k] - overlap.vec[k])){
#       temp.nonmatch[i,] <- unname(unlist(lapply(u.list, function(x){
#         rmultinom(1, 1, x)
#       })))
#     }
#
#     match.vectors[[k]] <- temp.match
#     nonmatch.vectors[[k]] <- temp.nonmatch
#   }
#
#   match.vectors <- do.call(rbind, match.vectors)
#   nonmatch.vectors <- do.call(rbind, nonmatch.vectors)
#
#   linkage.breaks <- c(0, cumsum(n2.vec))
#   overlap.breaks <- c(0, cumsum(overlap.vec))
#
#   df1matches <- sample(1:n1, overlap, replace = FALSE)
#
#   df2matches <- as.vector(unlist(mapply(function(x, y){
#     sample(x, y, replace = FALSE)
#   }, x = n2.lc, y = overlap.vec)))
#
#   pairs <- cbind(df1matches, df2matches)
#
#   Ztrue <- rep(n1 + 1, n2)
#   Ztrue[df2matches] <- df1matches
#
#   match.ids <- apply(pairs, 1, function(x){
#     which(ids[, 1] == x[1] & ids[, 2] == x[2])
#   })
#
#   comparisons <- matrix(NA, nrow = N, ncol = dim(m)[2])
#   comparisons[match.ids, ] <- match.vectors
#   comparisons[-match.ids, ] <- nonmatch.vectors
#   list(comparisons, n1, n2, levels, Ztrue, varying.fields, n1.vec, n2.vec)
# }
