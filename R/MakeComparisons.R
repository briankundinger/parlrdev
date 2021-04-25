# n <- 50
# ids <- expand.grid(1:n, 1:n)
# df1 <- sample(1:5, n, replace = TRUE)
# df2 <- sample(1:5, n, replace = TRUE)
# df1[ids[,1]] == df2[ids[,2]]

PossiblePatternsFS <- function(levels){
  levels %>%
    purrr::map(seq_len) %>%
    do.call(expand.grid, .) %>%
    unite(patterns_FS, seq_along(levels), sep = "")
}
