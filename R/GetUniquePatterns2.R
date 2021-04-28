GetUniquePatterns2 <- function(cd, fast = F, R = NULL){

  indicators <- cd[[1]]
  N <- dim(indicators)[1]
  fields <- dim(indicators)[2]
  n1 <- cd[[2]]
  n2 <- cd[[3]]

  ids <- expand.grid(1:n1, 1:n2)
  rec1 <- ids[,1]
  rec2 <- ids[,2]
  levels <- cd[[4]]
  # fields <- length(levels)
  # gk <- unlist(lapply(1:fields, function(x){
  #   0:(levels[x] -1)
  # }))
  # Lf_vec <- unlist(lapply(1:fields, function(x){
  #   rep(levels[x], levels[x])
  # }))
  #
  # fields_vec <- unlist(lapply(1:fields, function(x){
  #   rep(seq_len(fields)[x], levels[x])
  # }))


  #gk_ij <- sweep(indicators, MARGIN = 2, gk, FUN = "*")
  # Lf_long <- sapply(Lf_vec, function(x){
  #   rep(x, N)
  # })
  # fields_long <- sapply(fields_vec, function(x){
  #   rep(x, N)
  # })

  # level_sum <- cumsum(levels)
  # level_sum_vec <-  unlist(lapply(1:fields, function(x){
  #   rep(level_sum[x], levels[x])
  # }))
df <- data.frame(indicators, rec1, rec2)

pattern_df <- df %>%
  unite(pattern, 1:fields, sep = "")
  # level_sum_long <- sapply(level_sum_vec, function(x){
  #   rep(x, N)
  # })

  #hash <- rowSums((gk_ij >0) * 2 ^ (gk_ij + (fields_long > 1) * level_sum_long))
  # if(fast){
  #   numCores <- parallel::detectCores()
  #   cl <- parallel::makeCluster(numCores)
  #
  #   parallel::clusterExport(cl, c("gk", "fields_vec", "level_sum_vec"))
  #   hash_temp <- parallel::parApply(cl, indicators, 1, FUN = function(x){
  #     gk_ij <- x * gk
  #     (gk_ij >0) * 2 ^ (gk_ij + (fields_vec > 1) * level_sum_vec)
  #   })
  #
  #   parallel::stopCluster(cl)
  #
  # } else {
  #
  #   hash_temp <- apply(indicators, 1, function(x){
  #     gk_ij <- x * gk
  #     (gk_ij >0) * 2 ^ (gk_ij + (fields_vec > 1) * level_sum_vec)
  #   })


    # thing <- lapply(seq_len(nrow(indicators)), function(x){
    #   indicators[x, ]
    # })
    #
    # hash_temp <- lapply(thing, function(x){
    #   gk_ij <- x * gk
    #   (gk_ij >0) * 2 ^ (gk_ij + (fields_vec > 1) * level_sum_vec)
    # })

  #}
 # hash <- colSums(hash_temp)
  #hash_id <- as.numeric(factor(as.character(hash)))

  unique_hash <- unique(pattern_df$pattern)
  P <- length(unique_hash)

  hash_id <- vector(length = N)
  for(i in seq_along(unique_hash)){
    hash_id[pattern_df$pattern == unique_hash[i]] <- i
  }

  hash_id <- factor(hash_id)


  temp <- data.frame(indicators, rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()

  unique_patterns <- indicators[!duplicated(hash_id),]

  counts_by_rec <- temp %>%
    group_by(rec2, hash_id, .drop = F) %>%
    count()

  counts_by_rec <- split(counts_by_rec$n, counts_by_rec$rec2)

  thing <- split(temp, rec2)
  hash_to_rec1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
    })
    )
  hash_to_rec1 <- lapply(hash_to_rec1, function(x){
    append(x, list(n1 +1))
  })

  if(!is.null(R)){
    hash_to_rec1 <- lapply(hash_to_rec1, function(z){
    map(z, ~RandomIndexing(.x, R))
  })
  }


  patterns <- list(#hash_id,
                   unique_patterns,
                   pattern_counts,
                   counts_by_rec,
                   hash_to_rec1)

}


ProcessHash <- function(cd, fast = F, R = NULL){

  hash_id <- cd[[1]]
  N <- length(hash_id)
  #fields <- dim(indicators)[2]
  n1 <- cd[[2]]
  n2 <- cd[[3]]

  ids <- expand.grid(1:n1, 1:n2)
  rec1 <- ids[,1]
  rec2 <- ids[,2]
  levels <- cd[[4]]


  unique_patterns <- GetPossiblePatternsSad_sep(levels)
  P <- dim(unique_patterns)[1]

  hash_id <- factor(hash_id, levels = 1:P)


  temp <- data.frame(rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()


  counts_by_rec <- temp %>%
    group_by(rec2, hash_id, .drop = F) %>%
    count()

  counts_by_rec <- split(counts_by_rec$n, counts_by_rec$rec2)

  thing <- split(temp, rec2) #change to groupby
  hash_to_rec1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
  })
  )
  hash_to_rec1 <- lapply(hash_to_rec1, function(x){
    append(x, list(n1 +1))
  })

  if(!is.null(R)){
    hash_to_rec1 <- lapply(hash_to_rec1, function(z){
      map(z, ~RandomIndexing(.x, R))
    })
  }


  patterns <- list(#hash_id,
    unique_patterns,
    pattern_counts,
    counts_by_rec,
    hash_to_rec1)

}

ProcessHashBig <- function(cd, fast = F, R = NULL, Ztrue){

  hash_id <- cd[[1]]
  N <- length(hash_id)
  #fields <- dim(indicators)[2]
  n1 <- cd[[2]]
  n2_index <- cd[[3]]

  ids <- expand.grid(1:n1, n2_index)
  rec1 <- ids[,1]
  rec2 <- ids[,2]
  levels <- cd[[4]]


  unique_patterns <- GetPossiblePatternsSad_sep(levels)
  P <- dim(unique_patterns)[1]

  hash_id <- factor(hash_id, levels = 1:P)


  temp <- data.frame(rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()


  counts_by_rec <- temp %>%
    group_by(rec2, hash_id, .drop = F) %>%
    count()

  counts_by_rec <- split(counts_by_rec$n, counts_by_rec$rec2)

  thing <- split(temp, rec2) #change to groupby
  hash_to_rec1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
  })
  )
  hash_to_rec1 <- lapply(hash_to_rec1, function(x){
    append(x, list(n1 +1))
  })

  if(!is.null(R)){
    hash_to_rec1 <- lapply(hash_to_rec1, function(z){
      map(z, ~RandomIndexing(.x, R))
    })
  }


  patterns <- list(#hash_id,
    unique_patterns,
    pattern_counts,
    counts_by_rec,
    hash_to_rec1, Ztrue)

}

