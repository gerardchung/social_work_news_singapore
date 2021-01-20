# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# After training the model in do-file "news_sw_training.R", i now predict
# on the rest of the documents.
# The model "final_trained_model" in cr_data folder was trained on 1000 documents. 
# I predict using this model on about 344 remaining documents


rm(list = ls())

library(textreadr)
library(stringr)
library(dplyr)
getwd()

# load RData dataset
####################
load(file = "cr_data/sw_news.RData")

# Convert foldernum to numeric
sw.news$foldernum  <- as.numeric(sw.news$foldernum)

# create id 
sw.news <- sw.news %>%  
    mutate(id = row_number()) %>% 
    relocate(id, .before = foldernum)





