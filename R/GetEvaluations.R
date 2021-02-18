GetEvaluations <- function(Zhat, Ztrue, n1){
  # Zhat = Bayes Estimate of linkage structure (from BRL)
  # Ztrue = True linkage structure
  # n1 = size of larger file

  nLinks <- sum(Zhat <= n1)
  nMatches <- sum(Ztrue <= n1)
  nCorrectLinks <- sum(Zhat[Zhat<=n1]==Ztrue[Zhat<=n1])
  recall <- nCorrectLinks/nMatches
  precision <- nCorrectLinks/nLinks
  fmeasure <- 2 * (recall * precision) / (recall + precision)
  eval <- c(recall, precision, fmeasure)
  names(eval) <- c("Recall", "Precision", "Fmeasure")
  eval
}
