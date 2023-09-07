hash_field <- function(L_f, k, Lf_vec){
  level_seq <- seq_len(L_f)
  as.numeric(level_seq > 0) * 2 ^ ((level_seq) + (as.numeric(k > 1)  * Lf_vec[k]))
}

# vabl_hash <- function(comparisons, all_patterns = FALSE){
#
#   indicators <- comparisons[[1]]
#   N <- dim(indicators)[1]
#   n1 <- comparisons[[2]]
#   n2 <- comparisons[[3]]
#
#   levels <- comparisons[[4]]
#   fields <- seq_along(comparisons[[4]])
#   field_marker <- sapply(fields, function(x){
#     rep(x, comparisons[[4]][x])
#   }) %>%
#     unlist(.) %>%
#     as.vector(.)
#
#   ids <- expand.grid(1:n1, 1:n2)
#   rec1 <- ids[,1]
#   rec2 <- ids[,2]
#
#   Lf_vec<- (levels) %>%
#     c(0, .) %>%
#     cumsum()
#
#   hash_vals <- imap(comparisons[[4]], ~hash_field(.x, .y, Lf_vec)) %>%
#     unlist()
#
#   hash <- sweep(indicators, 2, hash_vals, "*") %>%
#     rowSums() + 1
#
#   if(all_patterns == TRUE){
#
#     unique_patterns <- GetPossiblePatternsSad_missing(levels)
#     unique_hashed <- sweep(unique_patterns, 2, hash_vals, "*") %>%
#       rowSums() + 1
#     P <- dim(unique_patterns)[1]
#     hash_id <- hash %>%
#       factor(levels = unique_hashed) %>%
#       as.integer() %>%
#       factor(levels = 1:P)
#
#   } else {
#
#     unique_hashed <- unique(hash)
#     P <- length(unique_hashed)
#
#     hash_id <- hash %>%
#       factor(levels = unique_hashed) %>%
#       as.integer() %>%
#       factor(levels = 1:P)
#     unique_patterns <- indicators[!duplicated(hash_id), ]
#   }
#
#   temp <- data.frame(indicators, rec1, rec2, hash_id)
#   pattern_counts <- temp %>%
#     group_by(hash_id, .drop = F) %>%
#     count() %>%
#     pull()
#
#   thing <- split(temp, rec2)
#
#   # This is the computational bottleneck
#   hash_to_file_1 <- map(thing, ~ lapply(1:P, function(y){
#     which(.x$hash_id == y)
#   })
#   )
#
#   pattern_counts_by_record <-  map(hash_to_file_1, ~lapply(.x, length)) %>%
#     map(unlist)
#
#   record_counts_by_pattern <- purrr::transpose(pattern_counts_by_record) %>%
#     map(unlist) %>%
#     map(unname)
#
#   # record_counts_by_pattern <- temp %>%
#   #   mutate(rec2 = factor(rec2, 1:n2, 1:n2)) %>%
#   #   split(., hash_id) %>%
#   #   lapply(., function(x){
#   #     x %>%
#   #       group_by(rec2, .drop = F) %>%
#   #       count() %>%
#   #       pull()
#   #   })
#
#   # NOTE: Compare memory costs of hash_to_file_1 and flags
#   flags <- lapply(hash_to_file_1, function(x){
#
#     eligible_patterns <- sapply(x, function(y){
#       length(y) == 1
#       }) %>%
#       which()
#
#     eligible_records <- sapply(eligible_patterns, function(y){
#       x[[y]]
#       })
#
#     data.frame(eligible_patterns, eligible_records)
#
#   })
#
#   patterns <- list(ohe = unique_patterns,
#                    total_counts = pattern_counts,
#                    pattern_counts_by_record = pattern_counts_by_record,
#                    record_counts_by_pattern = record_counts_by_pattern,
#                    flags = flags,
#                    field_marker = field_marker,
#                    n1 = n1,
#                    n2 = n2)
#   patterns
#
# }

HashComparisons <- function(comparisons,
                            method = "both",
                            all_patterns = FALSE){


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
  hash_to_file_1 <- map(thing, ~ lapply(1:P, function(y){
    which(.x$hash_id == y)
  })
  )

  pattern_counts_by_record <-  map(hash_to_file_1, ~lapply(.x, length)) %>%
    map(unlist)

  record_counts_by_pattern <- purrr::transpose(pattern_counts_by_record) %>%
    map(unlist) %>%
    map(unname)

  # NOTE: Compare memory costs of hash_to_file_1 and flags
  flags <- lapply(hash_to_file_1, function(x){

    eligible_patterns <- sapply(x, function(y){
      length(y) == 1
    }) %>%
      which()
    eligible_records <- sapply(eligible_patterns, function(y){
      x[[y]]
    })

    data.frame(eligible_patterns, eligible_records)

  })

  if(method == "vabl"){
    hash_to_file_1 <- NULL
  }

  if(method == "fabl"){
    flags <- NULL
  }

  patterns <- list(ohe = unique_patterns,
                   total_counts = pattern_counts,
                   pattern_counts_by_record = pattern_counts_by_record,
                   record_counts_by_pattern = record_counts_by_pattern,
                   hash_to_file_1 = hash_to_file_1,
                   flags = flags,
                   field_marker = field_marker,
                   n1 = n1,
                   n2 = n2)
  patterns

}

vabl_hash <- function(comparisons,
                            method = "both",
                            all_patterns = FALSE){


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
    unique_patterns <- indicators[!duplicated(hash_id), ]
  }

  temp <- data.frame(indicators, rec1, rec2, hash_id)
  pattern_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()

  pattern_lookup <- expand.grid(1:P, 1:n2) %>%
    data.frame() %>%
    setNames(., c("hash_id", "rec2"))

  hash_to_file_1 <- temp %>%
    select(rec1, rec2, hash_id) %>%
    nest_by(rec2, hash_id, .keep = F) %>%
    mutate(hash_id = as.integer(hash_id)) %>%
    rowwise() %>%
    mutate(N = nrow(data))

  hash_to_file_1 <- left_join(x = pattern_lookup,
                      y = hash_to_file_1,
                      by = c("hash_id", "rec2"))

  # counts <-  hash_to_file_1 %>%
  #   rowwise() %>%
  #   mutate(N = nrow(data)) %>%
  #   #select(-data)  %>%
  #   mutate(hash_id = as.integer(hash_id))
  #
  # counts <- left_join(x = pattern_lookup,
  #                    y = counts,
  #                    by = c("hash_id", "rec2"))

  hash_to_file_1$N[is.na(hash_to_file_1$N)] <- 0

  pattern_counts_by_record <- hash_to_file_1 %>%
    select(-data) %>%
    group_split(rec2, .keep = F) %>%
    map(., `[[`, "N")

  record_counts_by_pattern <- purrr::transpose(pattern_counts_by_record) %>%
    map(unlist) %>%
    map(unname)

  # thing <- hash_to_file_1 %>%
  #   group_split(rec2, .keep = F)

  flags <- hash_to_file_1 %>%
    filter(N ==1) %>%
    unnest(data) %>%
    complete(rec2 = unique(hash_to_file_1$rec2)) %>%
    select(-N) %>%
    # pull(rec2) %>%
    # unique() %>%
    # length() %>%
    setNames(c("rec2", "eligible_patterns", "eligible_records")) %>%
    group_split(rec2, .keep = F)

#   thing2 <- thing %>%
#     filter(is.na(N))
#
#
#   flags <- lapply(thing2, function(x){
#
#   eligible_marker <- x$N == 1
#   eligible_records <- x$data[eligible_marker] %>%
#     unlist() %>%
#     unname()
#
#   data.frame(eligible_patterns = x$hash_id[eligible_marker],
#              eligible_records = eligible_records)
#
#   })
# #
#   flags <- counts %>%
#     filter(N == 1) %>%
#     unnest(data) %>%
#     select(-N) %>%
#     setNames(c("eligible_patterns", "rec2", "eligible_records"))
#
#   flags <- flags %>%
#    group_split(rec2, .keep = F)


  # NOTE: Compare memory costs of hash_to_file_1 and flags
  # flags <- lapply(hash_to_file_1, function(x){
  #
  #   eligible_patterns <- sapply(x, function(y){
  #     length(y) == 1
  #   }) %>%
  #     which()
  #   eligible_records <- sapply(eligible_patterns, function(y){
  #     x[[y]]
  #   })
  #
  #   data.frame(eligible_patterns, eligible_records)
  #
  # })

  if(method == "vabl"){
    hash_to_file_1 <- NULL
  }

  if(method == "fabl"){
    flags <- NULL
  }

  patterns <- list(ohe = unique_patterns,
                   total_counts = pattern_counts,
                   pattern_counts_by_record = pattern_counts_by_record,
                   record_counts_by_pattern = record_counts_by_pattern,
                   hash_to_file_1 = hash_to_file_1,
                   flags = flags,
                   field_marker = field_marker,
                   n1 = n1,
                   n2 = n2)
  patterns

}

