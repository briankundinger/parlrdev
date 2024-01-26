# CombineHash_fabl <- function(hash_list, dfA, dfB){
#
#   n1 <- nrow(dfA)
#   n2 <- nrow(dfB)
#
#   total_counts <- Reduce(`+`, map(hash_list, ~.x$total_counts))
#
#   pattern_counts_by_record <- hash_list %>%
#     map(`[[`, "pattern_counts_by_record") %>%
#     flatten()
#
#   hash_to_file_1 <- hash_list %>%
#     map(`[[`, "hash_to_file_1") %>%
#     flatten()
#
#   patterns <- list(ohe = hash_list[[1]][[1]],
#                    total_counts = counts_big,
#                    pattern_counts_by_record = counts_per_rec_big,
#                    hash_to_file_1 = hash_to_file_1,
#                    field_marker = field_marker,
#                    n1 = n1,
#                    n2 = n2)
#
#
#   patterns
# }

CombineHash <- function(hash_list, dfA, dfB){

  n1 <- nrow(dfA)
  n2 <- nrow(dfB)

  total_counts <- Reduce(`+`, map(hash_list, ~.x$total_counts))

  pattern_counts_by_record <- hash_list %>%
    map(`[[`, "pattern_counts_by_record") %>%
    flatten()

  hash_to_file_1 <- hash_list %>%
    map(`[[`, "hash_to_file_1") %>%
    flatten()

  record_counts_by_pattern <- transpose(pattern_counts_by_record) %>%
    map(unlist) %>%
    map(unname)

  flags <- hash_list %>%
    map(`[[`, "flags") %>%
    flatten()

  patterns <- list(ohe = hash_list[[1]]$ohe,
                   total_counts = total_counts,
                   pattern_counts_by_record = pattern_counts_by_record,
                   record_counts_by_pattern = record_counts_by_pattern,
                   hash_to_file_1 = hash_to_file_1,
                   flags = flags,
                   field_marker = hash_list[[1]]$field_marker,
                   n1 = n1,
                   n2 = n2)

}
