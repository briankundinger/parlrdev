library(RecordLinkage)
library(dplyr)
library(stringr)
library(parlrdev)
library(purrr)
library(tidyverse)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

HashRecords <- function(df1, df2){
  cd <- CompareNLTCS(df1, df2)
  patterns <- GetUniquePatterns2(cd, R = 10)
  patterns
}

nltcs <- read.csv("data/proc_nltcs.csv")
df82 <- nltcs %>%
  filter(FILE == 82) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = str_sub(REC, start = 3L),
         DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = str_pad(STATE, 2, pad = "0"),
         REGOFF = str_pad(REGOFF, 2, pad = "0")) %>%
  unite(dob, 2:4, sep = "") %>%
  unite(location, c(STATE, REGOFF), sep = "")


df89 <- nltcs %>%
  filter(FILE == 89) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = str_sub(REC, start = 3L)) %>%
  mutate(unique_ID = str_sub(REC, start = 3L),
         DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = str_pad(STATE, 2, pad = "0"),
         REGOFF = str_pad(REGOFF, 2, pad = "0")) %>%
  unite(dob, 2:4, sep = "") %>%
  unite(location, c(STATE, REGOFF), sep = "")


chunks <- 30
chunk_size <- ceiling(dim(df89)[1]/chunks)
chunk_id <- rep(1:chunks, each = chunk_size)[1:dim(df89)[1]]

#job_marker <- rep(1:88, each =200)
#job_marker <- job_marker[1:nrow(df89)]
chunk_marker <- chunk_id == taskID
chunk89 <- df89[chunk_marker, ]

hash <- HashRecords(df82, chunk89)
saveRDS(hash, file = paste0("hash/dependent/", "hash_",
                            str_pad(taskID, 2, pad = "0")))
