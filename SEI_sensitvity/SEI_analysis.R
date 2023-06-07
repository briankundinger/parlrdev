

Z_paths <- list.files(path = "SEI_sensitvity/Zs", full.names = T)

thing <- readRDS("SEI_sensitvity/Zs/Zs_01")
Zs_list <- lapply(Z_paths, readRDS)

distinct_Z <- lapply(Zs_list, function(x){
  dim(unique(x, MARGIN = 2))[2]
})

eval_paths <- list.files(path = "SEI_sensitvity/eval", full.names = T)
eval_list <- lapply(eval_paths, readRDS)
recall_var <- lapply(eval_list, function(x){
  var(x[1, ])
})

precision_var <- lapply(eval_list, function(x){
  var(x[2, ])
})
