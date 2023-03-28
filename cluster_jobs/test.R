library(RecordLinkage)
library(dplyr)
library(stringr)

#setwd("cluster_jobs")
ApplyModel <- function(method){
  model <- readRDS(paste0("trained_models/", method))
  out <- classifySupv(model, cd)
  out$pairs %>%
    as.data.frame() %>%
    mutate(prediction = out$prediction[]) %>%
    summarize(total_declared = sum(prediction == "L"),
              true_declared = sum(is_match == 1 & prediction == "L"),
              false_declared = sum(is_match == 0 & prediction == "L")) %>%
    mutate(method = method,
           array_job = taskID)
}

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#taskID <- 3
# model_paths <- list.files("trained_models", full.names = T)
# models <- lapply(model_paths, readRDS)
methods <- list.files("trained_models", full.names = F)
cd_paths <- list.files("comp_vectors", full.names = T)
cd_path <- cd_paths[taskID]
cd <- readRDS(cd_path)

results <- lapply(methods, ApplyModel)
results_df <- do.call(rbind, results)
saveRDS(results_df, file = paste0("eval/eval_", taskID))
