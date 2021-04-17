RandomIndexing <- function(x, R){
  if(length(x) <= R){
    return(x)
  } else {
    sample(x, R, replace = FALSE)
  }
}
