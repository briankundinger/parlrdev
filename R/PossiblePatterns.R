FS_to_Sadinle <- function(gamma, levels){
  unname(unlist(mapply(function(z, y){
    gamma_f <- rep(0, y)
    if(z == 0){
      return(gamma_f)
    }
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

GetPossiblePatternsSad <- function(levels){
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

GetPossiblePatternsSad_sep <- function(levels){
  possible_values <- lapply(levels, function(x){
    seq_len(x)
  })
  possible_patterns <- data.frame(do.call(expand.grid, possible_values))

  thing <- data.frame(t(apply(possible_patterns, 1, function(x){
    FS_to_Sadinle(x, levels)
  })))
  thing
}

GetPossiblePatternsSad_missing <- function(levels){
  possible_values <- lapply(levels, function(x){
    c(0, seq_len(x))
  })
  possible_patterns <- data.frame(do.call(expand.grid, possible_values))

  thing <- data.frame(t(apply(possible_patterns, 1, function(x){
    FS_to_Sadinle(x, levels)
  })))
  thing
}

PossiblePatternsFS <- function(levels){
  levels %>%
    purrr::map(seq_len) %>%
    do.call(expand.grid, .) %>%
    unite(patterns_FS, seq_along(levels), sep = "")
}

GetPossiblePatternsFS_sep <- function(levels){
  levels %>%
    purrr::map(seq_len) %>%
    do.call(expand.grid, .)
}
