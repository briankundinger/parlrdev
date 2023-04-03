library(RecordLinkage)
library(dplyr)
library(stringr)

ncvr <- read.csv("data/ncvr.csv")
# ncvr_clean <- ncvr %>%
#   apply(., 2, function(x){
#     x[x == ""] <- NA
#     x
#   }) %>%
#   as.data.frame()


ncvr$full_phone_num[ncvr$full_phone_num == ""] <- NA
ncvr$middle_name[ncvr$middle_name == ""] <- NA

ncvr_clean <- ncvr
saveRDS(ncvr_clean, "data/ncvr_clean")

dfA <- ncvr_clean %>%
  filter(file_id == "a") %>%
  filter(city == "raleigh")

dfB <- ncvr_clean %>%
  filter(file_id == "b") %>%
  filter(city == "raleigh")

dfA <- dfA[!duplicated(dfA$voter_id), ]
dfB <- dfB[!duplicated(dfB$voter_id), ]

saveRDS(dfA, "data/raleigh_A")
saveRDS(dfB, "data/raleigh_B")

