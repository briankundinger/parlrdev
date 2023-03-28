library(RecordLinkage)
library(dplyr)
library(stringr)
library(italy)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
train_set <- 1:1250
id1 <- italy10$id[-train_set]
id2 <- italy08$id[-train_set]

df1 <- italy10 %>%
  select(-id, -PARENT)

df2 <- italy08 %>%
  select(-id, -PARENT)

df1 <- df1[-train_set, ]
df2 <- df2[-train_set, ]

job_marker <- rep(1:63, each =200)
job_marker <- job_marker[1:nrow(df2)]
chunk_marker <- job_marker == taskID

chunk2 <- df2[chunk_marker, ]

cd <- compare.linkage(df1, chunk2,
                identity1 = id1, identity2 = id2[chunk_marker])
saveRDS(cd, file = paste0("comp_vectors/", "cd_", taskID))
