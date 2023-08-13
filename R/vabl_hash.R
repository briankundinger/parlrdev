hash_field <- function(L_f, k, Lf_vec){
  level_seq <- seq_len(L_f)
  as.numeric(level_seq > 0) * 2 ^ ((level_seq) + (as.numeric(k > 1)  * Lf_vec[k]))
}

vabl_hash <- function(comparisons, all_patterns = FALSE){

  indicators <- comparisons[[1]]
  N <- dim(indicators)[1]
  n1 <- comparisons[[2]]
  n2 <- comparisons[[3]]

  levels <- comparisons[[4]]
  fields <- seq_along(comparisons[[4]])
  field_marker <- sapply(fields, function(x){
    rep(x, comparisons[[4]][x])
  }) %>%
    unlist(.) %>%
    as.vector(.)

  ids <- expand.grid(1:n1, 1:n2)
  rec1 <- ids[,1]
  rec2 <- ids[,2]

  Lf_vec<- (levels) %>%
    c(0, .) %>%
    cumsum()

  hash_vals <- imap(comparisons[[4]], ~hash_field(.x, .y, Lf_vec)) %>%
    unlist()

  hash <- sweep(indicators, 2, hash_vals, "*") %>%
    rowSums() + 1

  if(all_patterns == TRUE){

    unique_patterns <- GetPossiblePatternsSad_missing(levels)
    unique_hashed <- sweep(unique_patterns, 2, hash_vals, "*") %>%
      rowSums() + 1
    P <- dim(unique_patterns)[1]
    hash_id <- hash %>%
      factor(levels = unique_hashed) %>%
      as.integer() %>%
      factor(levels = 1:P)

  } else {

    unique_hashed <- unique(hash)
    P <- length(unique_hashed)

    hash_id <- hash %>%
      factor(levels = unique_hashed) %>%
      as.integer() %>%
      factor(levels = 1:P)
    unique_patterns <- indicators[!duplicated(hash_id), ]
  }

  temp <- data.frame(indicators, rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()

  thing <- split(temp, rec2)

  # This is the computational bottleneck
  hash_to_file1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
  })
  )

  pattern_counts_by_record <-  map(hash_to_file1, ~lapply(.x, length)) %>%
    map(unlist)

  record_counts_by_pattern <- temp %>%
    mutate(rec2 = factor(rec2, 1:n2, 1:n2)) %>%
    split(., hash_id) %>%
    lapply(., function(x){
      x %>%
        group_by(rec2, .drop = F) %>%
        count() %>%
        pull()
    })

  # NOTE: Compare memory costs of hash_to_file1 and flags
  flags <- lapply(hash_to_file1, function(x){

    eligible_patterns <- sapply(x, function(y){
      length(y) == 1
      }) %>%
      which()

    eligible_records <- sapply(eligible_patterns, function(y){
      x[[y]]
      })

    data.frame(eligible_patterns, eligible_records)

  })

  patterns <- list(ohe = unique_patterns,
                   total_counts = pattern_counts,
                   pattern_counts_by_record = pattern_counts_by_record,
                   record_counts_by_pattern = record_counts_by_pattern,
                   flags = flags,
                   field_marker = field_marker)
  patterns

}


