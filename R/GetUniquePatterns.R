GetUniquePatterns <- function(comparisons, fast = F){

  indicators <- comparisons[[1]]
  N <- dim(indicators)[1]
  levels <- comparisons[[4]]
  fields <- length(levels)
  gk <- unlist(lapply(1:fields, function(x){
    0:(levels[x] -1)
  }))
  Lf_vec <- unlist(lapply(1:fields, function(x){
    rep(levels[x], levels[x])
  }))

  fields_vec <- unlist(lapply(1:fields, function(x){
    rep(seq_len(fields)[x], levels[x])
  }))


  #gk_ij <- sweep(indicators, MARGIN = 2, gk, FUN = "*")
  # Lf_long <- sapply(Lf_vec, function(x){
  #   rep(x, N)
  # })
  # fields_long <- sapply(fields_vec, function(x){
  #   rep(x, N)
  # })

  level_sum <- cumsum(levels)
  level_sum_vec <-  unlist(lapply(1:fields, function(x){
    rep(level_sum[x], levels[x])
  }))

  # level_sum_long <- sapply(level_sum_vec, function(x){
  #   rep(x, N)
  # })

  #hash <- rowSums((gk_ij >0) * 2 ^ (gk_ij + (fields_long > 1) * level_sum_long))
  if(fast){
    numCores <- parallel::detectCores()
    cl <- parallel::makeCluster(numCores)

    parallel::clusterExport(cl, c("gk", "fields_vec", "level_sum_vec"))
    hash_temp <- parallel::parApply(cl, indicators, 1, FUN = function(x){
      gk_ij <- x * gk
      (gk_ij >0) * 2 ^ (gk_ij + (fields_vec > 1) * level_sum_vec)
    })

    parallel::stopCluster(cl)

  } else {

    hash_temp <- apply(indicators, 1, function(x){
      gk_ij <- x * gk
      (gk_ij >0) * 2 ^ (gk_ij + (fields_vec > 1) * level_sum_vec)
    })


    # thing <- lapply(seq_len(nrow(indicators)), function(x){
    #   indicators[x, ]
    # })
    #
    # hash_temp <- lapply(thing, function(x){
    #   gk_ij <- x * gk
    #   (gk_ij >0) * 2 ^ (gk_ij + (fields_vec > 1) * level_sum_vec)
    # })

  }
  hash <- colSums(hash_temp)
  #hash_id <- as.numeric(factor(as.character(hash)))

  unique_hash <- unique(hash)
  hash_id <- rep(0, N)
  for(i in seq_along(unique_hash)){
    hash_id[hash == unique_hash[i]] <- i
  }


  temp <- data.frame(indicators, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id) %>%
    count() %>%
    pull()

  unique_patterns <- indicators[!duplicated(hash),]

  # unique_patterns <- temp %>%
  #   unique() %>%
  #   arrange(hash_id) %>%
  #   select(-hash_id)

  patterns <- list(hash_id, unique_patterns, pattern_counts)

}
