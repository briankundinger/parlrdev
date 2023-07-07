library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)
library(readr)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(41)
files <- list.files(path = "data/SimulationDataFiles", full.names = T)
all_patterns = TRUE

m_prior = 1
u_prior = 1
alpha = 1
beta = 1
S = 500
iterations <- 100
burn = 100
show_progress = T
fast = F
all_patterns = TRUE


overlap_vals <- c(50, 250, 450)

j <-  2
i <-  296

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
R_vals <- c(1, 2, 5, 10, 20)
R <- R_vals[taskID]

Zs <- matrix(NA, nrow = n2, ncol = iterations)
evals <- matrix(NA, nrow = 3, ncol = iterations)

for(iter in 1:iterations){
  Zchain <- BKSimple_hash2(cd, R = R, S = S)
  Zhat <- LinkRecordsBK(Zchain[[1]], n1, 1, 1, 2, Inf)
  eval <- GetEvaluations(Zhat[[1]], Ztrue, n1)
  Zs[, iter] <- Zhat[[1]]
  evals[, iter] <- eval
}

saveRDS(Zs, file = paste0("out/Zs/Zs_", str_pad(R, 3, pad = "0")))
saveRDS(evals, file = paste0("out/eval/eval_", str_pad(R, 3, pad = "0")))
