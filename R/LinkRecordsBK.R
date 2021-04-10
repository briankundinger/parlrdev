LinkRecordsBK<- function(Zchain, n1, lFNM=1, lFM1=1, lFM2=2, lR=Inf){
  #
  # This is a complete copy of "linkrecords" from BRL, only modified
  # so that it passes on the posterior link probabilities
  #
  #
  #

  # control the input
  if(!is.matrix(Zchain)) stop("Zchain should be a matrix")
  n2 <- nrow(Zchain)
  # make sure the labels in Zchain are within the expected range
  if(max(Zchain) > n1 + n2) stop("Labels in Zchain exceed n1+n2")
  # - positive losses
  C0 <- (lFNM > 0) & (lFM1 > 0) & (lFM2 > 0) & (lR > 0)
  # - conditions of Theorem 1 of Sadinle (2017)
  C1 <- (lR == Inf) & (lFNM <= lFM1) & (lFNM + lFM1 <= lFM2)
  # - conditions of Theorem 2 of Sadinle (2017)
  C2 <- ((lFM2 >= lFM1) & (lFM1 >= 2*lR)) | ((lFM1 >= lFNM) & (lFM2 >= lFM1 + lFNM))
  # - conditions of Theorem 3 of Sadinle (2017)
  C3 <- (lFM2 >= lFM1) & (lFM1 >= 2*lR) & (lFNM >= 2*lR)
  # check we can handle the specified losses
  if(!C0) stop("Losses need to be positive")
  if(!any(c(C1,C2,C3))) stop("Invalid configuration of losses")

  # temporarily replace all nonlink labels by n1+1
  Zchain[Zchain > n1+1] <- n1+1
  tableLabels <- apply(Zchain, 1, tabulate, nbins=max(Zchain))
  tableLabels <- tableLabels/ncol(Zchain)
  probNoLink <- tableLabels[n1+1,]
  # find marginal best option for each record based only on probability
  maxProbOption <- apply(tableLabels, 2, which.max)
  maxProbOption[maxProbOption==n1+1] <- (n1+1:n2)[maxProbOption==n1+1]
  probMaxProbOption <- apply(tableLabels, 2, max)
  maxProbOptionIsLink <- maxProbOption <= n1

  if(C1){# if not using reject option and conditions of Theorem 1

    Zhat <- (n1+1):(n1+n2)
    tholdLink <- lFM1/(lFM1+lFNM) +
      (lFM2-lFM1-lFNM)*(1 - probNoLink - probMaxProbOption)/(lFM1+lFNM)
    Zhat[maxProbOptionIsLink & (probMaxProbOption > tholdLink)] <-
      maxProbOption[maxProbOptionIsLink & (probMaxProbOption > tholdLink)]

  }else{# if using reject option
    if(C3){# if conditions of Theorem 3 are satisfied

      Zhat <- rep(-1,n2) # represents the reject option
      tholdLink <- 1 - lR/lFM1 + (lFM2-lFM1)*(1 - probNoLink - probMaxProbOption)/lFM1
      Zhat[maxProbOptionIsLink & (probMaxProbOption > tholdLink) ] <-
        maxProbOption[maxProbOptionIsLink & (probMaxProbOption > tholdLink) ]
      noLinkDec <- probNoLink > 1-lR/lFNM
      Zhat[noLinkDec] <- ((n1+1):(n1+n2))[noLinkDec]

    }else{ # Theorem 2

      # compute equation (6) in Sadinle (2017)
      tableLabels[-n1-1,] <- t( lFM2*(t(1-tableLabels[-n1-1,])-tableLabels[n1+1,]) +
                                  lFM1*tableLabels[n1+1,] )
      tableLabels[n1+1,] <- lFNM*(1-tableLabels[n1+1,])
      # find the options with the marginal minimal loss
      lossMinLossOption <- apply(tableLabels, 2, min)
      minLossOption <- apply(tableLabels, 2, which.min)
      noLinkDec <- minLossOption == n1+1
      minLossOption[noLinkDec] <- ((n1+1):(n1+n2))[noLinkDec]
      Zhat <- rep(-1,n2) # represents the reject option
      Zhat[lossMinLossOption < lR] <- minLossOption[lossMinLossOption < lR]

    }
  }

  return(list(Zhat = Zhat,
              post_probs = probMaxProbOption))
}
