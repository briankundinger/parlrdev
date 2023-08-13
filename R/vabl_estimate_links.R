vabl_estimate_links <- function(out, hash, resolve = TRUE){

  n2 <- hash$n2
  pattern_probs <- lapply(1:n2, function(j){
    out$pattern_weights/out$C[j]
  })

  possible_records <- lapply(1:n2, function(j){
    record <- c(hash$flags[[j]]$eligible_records, 0)
    prob <- c(pattern_probs[[j]][hash$flags[[j]]$eligible_patterns],
              exp(digamma(out$b_pi)) / out$C[j])

    data.frame(record, prob)
  })

  Zhat <- lapply(1:n2, function(j){
    best_match <- possible_records[[j]][possible_records[[j]]$prob > .5, ]
    if(nrow(best_match) == 0){
      record <- 0
      prob <- exp(digamma(out$b_pi)) / out$C[j]
      best_match <- data.frame(record, prob)
    }
    best_match
  }) %>%
    do.call(rbind, .)

  if(resolve == TRUE){
  double_matches <- Zhat$Z[duplicated(Zhat$record) & Zhat$Z != 0]
  to_resolve <- unlist(lapply(double_matches, function(x){
    dfB_options <- which(Zhat$record == x)
    dfB_probs <- post_probs[dfB_options]
    non_matches <- dfB_options[-which.max(dfB_probs)]
    non_matches
  }))
  Zhat$record[to_resolve] <- 0
  Zhat$prob[to_resolve] <- NA
  }

  list(Zhat = Zhat$record,
       prob = Zhat$prob)

}


# Zhat <- lapply(1:n2, function(j){
#   best_match <- hash$flags[[j]] %>%
#     mutate(p = pattern_probs[[j]][eligible_patterns]) %>%
#     filter(p > .5)
#
#   if(dim(best_match)[1] == 0){
#     Z <- 0
#     prob <- exp(digamma(out$b_pi)) / out$C[j]
#   } else {
#     Z <- best_match$eligible_records
#     prob <- best_match$p
#   }
#   data.frame(Z, prob)
# }) %>%
#   do.call(rbind, .)
