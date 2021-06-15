CompareBinary <- function(df1, df2, cols){
  nA <- dim(df1)[1]
  nB <- dim(df2)[1]
  ids <- expand.grid(1:nA, 1:nB)

  indicators <- lapply(cols, function(x){
    match <- df1[ids[,1], x] == df2[ids[,2], x]
    cbind(match, 1 - match)
  })

  indicators <- do.call(cbind, indicators)
  cd <- list(comparions = indicators,
             n1 = nA,
             n2 = nB,
             nDisagLevs = rep(2, length(cols)),
             Ztrue = NULL)
  cd
}
