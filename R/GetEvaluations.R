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

GetAverageLinkProb <- function(Zchain, record_cluster){
  Zchain[Zchain > n1+1] <- n1+1
  tableLabels <- apply(Zchain, 1, tabulate, nbins=max(Zchain))
  tableLabels <- tableLabels/ncol(Zchain)
  probNoLink <- tableLabels[n1+1,]
  # find marginal best option for each record based only on probability
  maxProbOption <- apply(tableLabels, 2, which.max)
  maxProbOption[maxProbOption==n1+1] <- (n1+1:n2)[maxProbOption==n1+1]
  probMaxProbOption <- apply(tableLabels, 2, max)
  maxProbOptionIsLink <- maxProbOption <= n1

  probMaxProbOption[!maxProbOptionIsLink] <- NA
  thing <- split(probMaxProbOption, record_cluster)
  sapply(thing, function(x){
    mean(x, na.rm = TRUE)
  })
}
