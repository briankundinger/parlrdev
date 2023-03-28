library(RecordLinkage)
library(dplyr)
library(stringr)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

nltcs <- read.csv("data/proc_nltcs.csv")
df82 <- nltcs %>%
  filter(FILE == 82) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = str_sub(REC, start = 3L),
         DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = str_pad(STATE, 2, pad = "0"),
         REGOFF = str_pad(REGOFF, 2, pad = "0"))
  #unite(dob, 2:4, sep = "") %>%
  #unite(location, c(STATE, REGOFF), sep = "")


df89 <- nltcs %>%
  filter(FILE == 89) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = str_sub(REC, start = 3L)) %>%
  mutate(unique_ID = str_sub(REC, start = 3L),
         DOB_DAY = str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = str_pad(STATE, 2, pad = "0"),
         REGOFF = str_pad(REGOFF, 2, pad = "0"))

df82 <- df82[-c(1:2000), ]
df89 <- df89[-c(1:2000), ]

job_marker <- rep(1:78, each =200)
job_marker <- job_marker[1:nrow(df89)]
chunk_marker <- job_marker == taskID

chunk89 <- df89[chunk_marker, ]

cd <- compare.linkage(df82[, -c(7, 8)], chunk89[, -c(7, 8)],
                identity1 = df82$unique_ID, identity2 = chunk89$unique_ID)
saveRDS(cd, file = paste0("comp_vectors/", "cd_", taskID))
