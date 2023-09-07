hash_field <- function(L_f, k, Lf_vec){
  level_seq <- seq_len(L_f)
  as.numeric(level_seq > 0) * 2 ^ ((level_seq) + (as.numeric(k > 1)  * Lf_vec[k]))
}

GetUniquePatterns2 <- function(cd,
                               fast = F,
                               R = NULL,
                               all_patterns = TRUE,
                               nA_index = FALSE,
                               df1_index = NULL,
                               df2_index = NULL){

  indicators <- cd[[1]]
  N <- dim(indicators)[1]
  fields <- dim(indicators)[2]
  n1 <- cd[[2]]
  n2 <- cd[[3]]

  # if(is.null(df1_index)){
  #   chunkA_index <- 1:n1
  # }else{
  #   chunkA_index <-  df1_index
  # }
  # if(is.null(df2_index)){
  #   chunkB_index <- 1:n2
  # }else{
  #   chunkB_index <-  df2_index
  # }
  #ids <- expand.grid(chunkA_index, chunkB_index)
  ids <- expand.grid(1:n1, 1:n2)
  rec1 <- ids[,1]
  rec2 <- ids[,2]
  levels <- cd[[4]]

#   Lf_vec<- (levels - 1) %>%
#     c(0, .) %>%
#     .[seq_along(levels)] %>%
#     cumsum()
#
#   hash_vals <- imap(cd[[4]], ~hash_field(.x, .y, Lf_vec)) %>%
#     unlist()
#
#   hash <- sweep(indicators, 2, hash_vals, "*") %>%
#     rowSums() + 1
#
# if(all_patterns == TRUE){
#
#   unique_patterns <- GetPossiblePatternsSad_sep(levels)
#   unique_hashed <- sweep(unique_patterns, 2, hash_vals, "*") %>%
#     rowSums() + 1
#   P <- dim(unique_patterns)[1]
#   hash_id <- hash %>%
#     factor(levels = unique_hashed) %>%
#     as.integer() %>%
#     factor(levels = 1:P)

  Lf_vec<- (levels) %>%
    c(0, .) %>%
    #.[seq_along(levels)] %>%
    cumsum()

  hash_vals <- imap(cd[[4]], ~hash_field(.x, .y, Lf_vec)) %>%
    unlist()

  hash <- sweep(indicators, 2, hash_vals, "*") %>%
    rowSums() + 1

  if(all_patterns == TRUE){
    ptm <- proc.time()

    unique_patterns <- GetPossiblePatternsSad_missing(levels)
    unique_hashed <- sweep(unique_patterns, 2, hash_vals, "*") %>%
      rowSums() + 1
    P <- dim(unique_patterns)[1]
    # hash_id <- hash %>%
    #   factor(levels = unique_hashed) %>%
    #   as.integer() %>%
    #   factor(levels = 1:P)
    hash_id <- match(hash, unique_hashed)

} else {

  unique_hashed <- unique(hash)
  P <- length(unique_hashed)

  # hash_id <- hash %>%
  #   factor(levels = unique_hashed) %>%
  #   as.integer() %>%
  #   factor(levels = 1:P)
  hash_id <- match(hash, unique_hashed)
  unique_patterns <- indicators[!duplicated(hash_id),]
}
  # hash_id <- (hash_id + 1) %>%
  #   factor(1:P_star)


  temp <- data.frame(indicators, rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()

  # counts_by_rec <- temp %>%
  #   group_by(rec2, hash_id, .drop = F) %>%
  #   count()
  #
  # counts_by_rec <- split(counts_by_rec$n, counts_by_rec$rec2)

  thing <- split(temp, rec2)
  hash_to_file_1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
    })
    )

  counts_by_rec <-  map(hash_to_file_1, ~lapply(.x, length)) %>%
    map(unlist)

  hash_to_file_1 <- lapply(hash_to_file_1, function(x){
    append(x, list(n1 +1))
  })


  if(!is.null(R)){
    hash_to_file_1 <- lapply(hash_to_file_1, function(z){
    map(z, ~RandomIndexing(.x, R))
  })
  }


  patterns <- list(ohe = unique_patterns,
                   total_counts = pattern_counts,
                   pattern_counts_by_record = counts_by_rec,
                   hash_to_file_1 = hash_to_file_1)
  patterns

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
  hash_to_file_1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
  })
  )
  hash_to_file_1 <- lapply(hash_to_file_1, function(x){
    append(x, list(n1 +1))
  })

  if(!is.null(R)){
    hash_to_file_1 <- lapply(hash_to_file_1, function(z){
      map(z, ~RandomIndexing(.x, R))
    })
  }


  patterns <- list(#hash_id,
    unique_patterns,
    pattern_counts,
    counts_by_rec,
    hash_to_file_1)

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
  hash_to_file_1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
  })
  )
  hash_to_file_1 <- lapply(hash_to_file_1, function(x){
    append(x, list(n1 +1))
  })

  if(!is.null(R)){
    hash_to_file_1 <- lapply(hash_to_file_1, function(z){
      map(z, ~RandomIndexing(.x, R))
    })
  }


  patterns <- list(#hash_id,
    unique_patterns,
    pattern_counts,
    counts_by_rec,
    hash_to_file_1, Ztrue)

}

