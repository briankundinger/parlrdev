ResolveConflicts <- function(Zhat, lR = Inf){

  post_probs <- Zhat[[2]]
  Zhat <- Zhat[[1]]
  double_matches <- Zhat[duplicated(Zhat) & Zhat != -1]
  if (lR == Inf){
    to_resolve <- unlist(lapply(double_matches, function(x){
      dfB_options <- which(Zhat == x)
      dfB_probs <- post_probs[dfB_options]
      non_matches <- dfB_options[-which.max(dfB_probs)]
      non_matches
    }))
    Zhat[to_resolve] <- max(Zhat)
    return(Zhat)
  } else {
    to_resolve <- unlist(lapply(double_matches, function(x){
      dfB_options <- which(Zhat == x)
      dfB_options
    }))
    Zhat[to_resolve] <- -1
    return(Zhat)

  }
}

