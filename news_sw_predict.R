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


# PREDICT TRAINED MODEL ON REMAINING DATASET
#############################################


# Join in handcoded dataset with 1000 observation
# ===============================================

# Merge in the handcoded labels "classify"
library(readxl)
handcoded <- read_xlsx("cr_data/sw_news_randomsample1000_coded.xlsx", 
                       col_names = T, 
)

tail(handcoded, n=5) # check last five rows
table(handcoded$classify) 

handcoded <- handcoded %>% select(id, title, classify) # keep 3 vars 

summary(handcoded)
summary(sw.news)

sw.news.labelled <- left_join(sw.news, handcoded, by = c("id", "title"))

glimpse(sw.news.labelled)
summary(sw.news.labelled)
table(sw.news.labelled$classify, exclude = NULL)

View_sw.news.labelled <- sw.news.labelled %>% 
    select(id, title, classify)


# Load in final.train.model 
# ==========================
    # load in final.train.model to pred on remaining 344 docs
load(file = "cr_data/final_trained_model", verbose = T)

# Prediction
sw.news.predict <- sw.news.labelled %>% 
    select(id, title, text, ends_with("_pred"), classify) %>% 
    filter(is.na(classify))

p <- predict(final.train.model, sw.news.predict, type = "prob")
head(p, n=10)
head(p, n=50)
class(p)


# Merge in the variable into sw.news.predict
    # p created by predict command is a dataframe with n and y variables
    # so i bring p$y into sw.news.predict as a new var
sw.news.predict$p_yes <- p$y
summary(sw.news.predict$p_yes)


# Code varuable classify
sw.news.predict <- sw.news.predict %>% 
    mutate(classify = replace(classify, p_yes > .5, "y")) %>% 
    mutate(classify = replace(classify, p_yes <= .5, "n"))

table(sw.news.predict$classify)
91+253
94+250

# Save into excel for checking on predicted docs
xlsx::write.xlsx(sw.news.predict, "cr_data/sw_news_checkingprediction344_2.xlsx")


# MERGE BACK IN WITH THE MAIN DATAFRAME 
#########################################

glimpse(sw.news.predict)
sw.news.predict <- select(sw.news.predict, id, title, text, geographic_pred, classify)

table(sw.news.labelled$classify, exclude = F)

sw.news.labelled_full <- left_join(sw.news.labelled, sw.news.predict, by = c("id", "title"))
table(sw.news.labelled_full$classify.x) # 300+700
table(sw.news.labelled_full$classify.y) # 91 + 253   / new = 94 250

sw.news.labelled_full <- sw.news.labelled_full %>% 
    mutate(classify = case_when(classify.x == "y" ~ "y", 
                                classify.x == "n" ~ "n",
                                classify.y == "y" ~ "y",
                                classify.y == "n" ~ "n")) 
table(sw.news.labelled_full$classify)
table(sw.news.labelled_full$classify.x)
table(sw.news.labelled_full$classify.y)
391+953
394 + 950
sw.news.labelled_full <- sw.news.labelled_full %>% 
    select(-classify.x, -classify.y)


# SAVE DATASET FOR ANALYSIS
############################
############################

save(sw.news.labelled_full, file = "an_data/an_sw_news.RData") 





