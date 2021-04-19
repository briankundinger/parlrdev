GetUniquePatterns3 <- function(cd, fast = F, R = NULL){

  indicators <- cd[[1]]
  N <- dim(indicators)[1]
  fields <- dim(indicators)[2]
  n1 <- cd[[2]]
  n2 <- cd[[3]]
  levels <- cd[[4]]
  possible_patterns <- GetPossiblePatterns(levels)

  ids <- expand.grid(1:n1, 1:n2)
  rec1 <- ids[,1]
  rec2 <- ids[,2]

  df <- data.frame(indicators, rec1, rec2)

  pattern_df <- df %>%
    unite(pattern, 1:fields, sep = "")




  # unique_hash <- unique(pattern_df$pattern)
   P <- length(possible_patterns)

  hash_id <- vector(length = n1 * n2)
  for(i in 1:P){
    hash_id[pattern_df$pattern == possible_patterns[i]] <- i
  }

  hash_id <- factor(hash_id)


  temp <- data.frame(indicators, rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id) %>%
    count() %>%
    pull()

  #unique_patterns <- indicators[!duplicated(hash_id),]

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
    possible_patterns,
    pattern_counts,
    counts_by_rec,
    hash_to_rec1)

}
