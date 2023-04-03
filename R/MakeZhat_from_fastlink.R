MakeZhat_from_fastlink <- function(idA, idB){
  Zhat <- rep(nA + 1, nB)
  Zhat[idB] <- idA
  Zhat
}
