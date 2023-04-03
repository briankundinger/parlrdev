MakeZtrue <- function(idA, idB, nA, nB){
  sapply(idB, function(x){
    match <- which(idA == x)
    if (length(match) == 0){
      match <- length(idA) + 1
    }
    match
  }) %>%
    unname()
}
