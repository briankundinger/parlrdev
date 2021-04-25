FS_to_Sadinle <- function(gamma, levels){
  unname(unlist(mapply(function(z, y){
    gamma_f <- rep(0, y)
    gamma_f[z] <- 1
    gamma_f
  }, z = gamma, y = levels)))
}

FS_to_Sadinle2 <- function(gamma_f, Lf){
  new_gamma <- matrix(0, nrow = length(gamma_f), ncol = Lf)
  for(i in seq_along(gamma_f)){
    new_gamma[i, gamma_f[i]+1] <- 1
  }
  new_gamma
}

GetPossiblePatterns <- function(levels){
  possible_values <- lapply(levels, function(x){
    seq_len(x)
  })
  possible_patterns <- data.frame(do.call(expand.grid, possible_values))

  thing <- data.frame(t(apply(possible_patterns, 1, function(x){
  FS_to_Sadinle(x, levels)
  })))
  thing %>%
    unite(patterns, 1:dim(.)[2], sep = "") %>%
    unlist() %>%
    unname()
}
