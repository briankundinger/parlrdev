library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)
library(readr)
library(BRL)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
i = taskID
set.seed(41)
files <- list.files(path = "data/SimulationDataFiles", full.names = T)

m_prior = 1
u_prior = 1
alpha = 1
beta = 1
S = 1000
burn = 100
show_progress = F
fast = F
R = NULL
all_patterns = TRUE
tmax= 200
threshold = 1e-8
resolve = T


overlap_vals <- c(50, 250, 450)

vabl_acc_samps <- matrix(NA, nrow = 3, ncol = 6)

fabl_acc_samps <- matrix(NA, nrow = 3, ncol = 6)
fabl_acc_samps_resolved <- matrix(NA, nrow = 3, ncol = 6)
sad_acc_samps <- matrix(NA, nrow = 3, ncol = 6)

fabl_acc_samps2 <- matrix(NA, nrow = 3, ncol = 6)
fabl_acc_samps_resolved2 <- matrix(NA, nrow = 3, ncol = 6)
sad_acc_samps2 <- matrix(NA, nrow = 3, ncol = 6)

for(j in seq_along(overlap_vals)){

  overlap <- overlap_vals[j]

  records <- read_csv(files[i], col_types = cols())
  records$file <- rep(2:1, length.out = dim(records)[1])

  records <- records %>%
    janitor::clean_names() %>%
    mutate(rec_id = as.numeric(str_extract(rec_id, "\\d{3}")) + 1)

  n1 <- 500
  n2 <- 500
  #overlap <- n2/2

  Ztrue <- n1 + 1:n2
  Ztrue[1:overlap] <- 1:overlap

  file1 <- records %>%
    filter(file ==1,
           rec_id <= n1) %>%
    select(-rec_id) %>%
    as.matrix(.) %>%
    data.frame(.) %>%
    mutate(occup = as.numeric(occup))

  file2 <- records %>%
    filter(file == 2,
           rec_id %in% c(1:overlap, (n1 +1):(1000 - overlap))) %>%
    select(-rec_id) %>%
    as.matrix() %>%
    data.frame(.) %>%
    mutate(occup = as.numeric(occup))


  cd <- suppressMessages(BRL::compareRecords(file1, file2, c(2, 3, 5, 6),
                                             types = c("lv", "lv", "bi", "bi")))
  cd[[1]] <- apply(cd[[1]], 2, as.numeric)


  hash <- vabl_hash(cd, all_patterns)
  ptm <- proc.time()
  out <- vabl_efficient(cd, threshold, tmax)
  elapsed <- proc.time() - ptm
  result <- vabl_estimate_links(out, hash, resolve = T)
  eval <- GetEvaluations(result$Zhat, Ztrue, n1)
  vabl_acc_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  #parlr method
  ptm <- proc.time()
  Zchain <- fabl_gibbs(cd, R = R)
  elapsed <- proc.time() - ptm
  Zhat <- LinkRecordsBK(Zchain[[1]], n1, 1, 1, 2, Inf)
  eval <- GetEvaluations(Zhat[[1]], Ztrue, n1)
  eval

  # Resolution Step
  Zhat_resolved <- ResolveConflicts(Zhat)
  eval_resolved <- GetEvaluations(Zhat_resolved, Ztrue, n1)
  fabl_acc_samps_resolved[j, ] <- c(eval_resolved, NA, elapsed[3], overlap)
  fabl_acc_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  #Partial Estimate
  Zhat <- LinkRecordsBK(Zchain[[1]], n1, 1, 1, 2, .1)
  Zhat <- ResolveConflicts(Zhat)
  eval <- GetEvaluations(Zhat, Ztrue, n1)
  RR <- sum(Zhat == -1)/n2
  eval[1] <- sum(Zhat == Ztrue & Ztrue > n1) / (sum(Zhat > n1))
  fabl_acc_samps2[j, ] <- c(eval, RR, elapsed[3], overlap)

  #Sadinle 2017 Method
  ptm <- proc.time()
  Zchain <- BRL::bipartiteGibbs(cd)[[1]]
  Zchain <- Zchain[,101:1000]
  elapsed <- proc.time() - ptm
  Zhat <- BRL::linkRecords(Zchain, n1, 1, 1, 2, Inf)
  eval <- GetEvaluations(Zhat, Ztrue, n1)
  sad_acc_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  Zhat <- BRL::linkRecords(Zchain, n1, 1, 1, 2, .1)
  RR <- sum(Zhat == -1)/n2
  eval <- GetEvaluations(Zhat, Ztrue, n1)
  eval[1] <- sum(Zhat == Ztrue & Ztrue > n1) / (sum(Zhat > n1))
  sad_acc_samps2[j, ] <- c(eval, RR, elapsed[3], overlap)

  #  print(i)
  #}
}

vabl_acc_samps <- data.frame(vabl_acc_samps, "vabl") %>%
  unname() %>%
  data.frame()

fabl_acc_samps <- data.frame(fabl_acc_samps, "fabl") %>%
  unname() %>%
  data.frame()
fabl_acc_samps_resolved <- data.frame(fabl_acc_samps_resolved, "fabl_resolved") %>%
  unname() %>%
  data.frame()
fabl_acc_samps2 <- data.frame(fabl_acc_samps2, "fabl_partial") %>%
  unname() %>%
  data.frame()
sad_acc_samps <- data.frame(sad_acc_samps, "BRL") %>%
  unname() %>%
  data.frame()
sad_acc_samps2 <- data.frame(sad_acc_samps2, "BRL_partial") %>%
  unname() %>%
  data.frame()

result_df <- rbind(vabl_acc_samp, fabl_acc_samps,
                   fabl_acc_samps_resolved, fabl_acc_samps2,
                   sad_acc_samps, sad_acc_samps2)

if(i < 100){
  error <- "One Error"
} else if (i < 200) {
  error <- "Two Errors"
} else {
  error <- "Three Errors"
}
names(result_df) <- c("recall", "precision", "f-Measure", "RR", "time", "overlap", "method")
result_df$Error <- error

saveRDS(result_df, file = paste0("out/sim_acc/sim_acc_",
                                      str_pad(i, 3, pad = "0")))

# saveRDS(fabl_acc_samps, file = paste0("out/fabl_acc/fabl_acc_",
#                           str_pad(i, 3, pad = "0")))
# saveRDS(fabl_acc_samps_resolved, file = paste0("out/fabl_acc_res/fabl_acc_res_",
#                           str_pad(i, 3, pad = "0")))
# saveRDS(sad_acc_samps, file = paste0("out/sad_acc/sad_acc_",
#                           str_pad(i, 3, pad = "0")))
# saveRDS(fabl_acc_samps2, file = paste0("out/fabl_acc2/fabl_acc2_",
#                           str_pad(i, 3, pad = "0")))
# saveRDS(sad_acc_samps2, file = paste0("out/sad_acc2/sad_acc2_",
#                           str_pad(i, 3, pad = "0")))
