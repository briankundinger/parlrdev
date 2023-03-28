library(RecordLinkage)
library(dplyr)
library(stringr)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

methods <- c("svm", "rpart")
method <- methods[taskID]
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

train82 <- df82[1:2000, ]
train89 <- df89[1:2000, ]


cd <- compare.linkage(train82[, -c(7, 8)], train89[, -c(7, 8)],
                      identity1 = train82$unique_ID, identity2 = train89$unique_ID)

model <- trainSupv(cd, method = paste(method))
saveRDS(model, file = paste0("trained_models/", method))
