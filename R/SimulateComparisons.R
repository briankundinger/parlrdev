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
  list(indicators, n1, n2, levels, Ztrue)
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


SimulateComparisonsLC2 <- function(m, u, levels, varying.fields,
                                  n1, n2, n1.vec, n2.vec, overlap.vec){
  #
  #
  #
  #
  #
  N <- n1*n2
  ids <- expand.grid(1:n1, 1:n2)
  parameter.indicators <- as.vector(unlist(sapply(1:length(levels), function(x){
    rep(x, levels[x])
  })))

  K <- dim(m)[1]
  N.vec <- n2.vec * n1;
  overlap <- sum(overlap.vec)

  # n1.breaks <- c(0, cumsum(n1.vec))
  n2.breaks <- c(0, cumsum(n2.vec))
  # n1.lc <- lapply(1:K, function(x){
  #   (n1.breaks[x]+1):n1.breaks[x+1]
  # })
  n2.lc <- lapply(1:K, function(x){
    (n2.breaks[x]+1):n2.breaks[x+1]
  })

  lc <- as.vector(sapply(1:K, function(x){
    rep(x, n2.vec[x])
  }))

  lc.ids <- split(1:n2, lc)
  lc.info <- NULL
  for(k in 1:K){
    lc.info[[k]] <- list(lc.ids[[k]], overlap.vec[k])
  }

  lc.long <- as.vector(sapply(1:K, function(x){
    rep(rep(x, n2.vec[x]), n1)
  }))

  match.vectors <- nonmatch.vectors <- NULL
  for(k in 1:K){
    temp.match <- matrix(NA, nrow = overlap.vec[k], ncol = dim(m)[2])
    temp.nonmatch <- matrix(NA, nrow = N.vec[k] - overlap.vec[k], ncol = dim(m)[2])
    m.list <- split(m[k, ], parameter.indicators)
    u.list <- split(u[k, ], parameter.indicators)


    for(i in 1:overlap.vec[k]){
      temp.match[i,]<- unname(unlist(lapply(m.list, function(x){
        rmultinom(1, 1, x)
      })))
    }

    for(i in 1:(N.vec[k] - overlap.vec[k])){
      temp.nonmatch[i,] <- unname(unlist(lapply(u.list, function(x){
        rmultinom(1, 1, x)
      })))
    }

    match.vectors[[k]] <- temp.match
    nonmatch.vectors[[k]] <- temp.nonmatch
  }

  match.vectors <- do.call(rbind, match.vectors)
  nonmatch.vectors <- do.call(rbind, nonmatch.vectors)

  linkage.breaks <- c(0, cumsum(n2.vec))
  overlap.breaks <- c(0, cumsum(overlap.vec))

  df1matches <- sample(1:n1, overlap, replace = FALSE)

  df2matches <- as.vector(unlist(mapply(function(x, y){
    sample(x, y, replace = FALSE)
  }, x = n2.lc, y = overlap.vec)))

  pairs <- cbind(df1matches, df2matches)

  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches

  match.ids <- apply(pairs, 1, function(x){
    which(ids[, 1] == x[1] & ids[, 2] == x[2])
  })

  comparisons <- matrix(NA, nrow = N, ncol = dim(m)[2])
  comparisons[match.ids, ] <- match.vectors
  comparisons[-match.ids, ] <- nonmatch.vectors
  list(comparisons, n1, n2, levels, Ztrue, varying.fields, n1.vec, n2.vec)
}
