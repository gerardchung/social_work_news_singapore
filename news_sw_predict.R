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
load(file = "cr_data/swnews_withpreds.RData")


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
summary(swnews_withpreds)

sw.news.labelled <- left_join(swnews_withpreds, handcoded, by = c("id", "title"))

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

p <- predict(final_trained_model, sw.news.predict, type = "prob")
    # if type = "raw", predict will auto use .50 threshold
head(p, n=10)
head(p, n=50)
class(p)

#p_raw <- predict(final.train.model, sw.news.predict, type = "raw")
#p_options <- tibble(p_raw, p)
#View(p_options)

# Merge in the variable into sw.news.predict
    # p created by predict command is a dataframe with n and y variables
    # so i bring p$y into sw.news.predict as a new var
sw.news.predict$p_yes <- p$y
summary(sw.news.predict$p_yes)


# Code variable classify
sw.news.predict <- sw.news.predict %>% 
    mutate(classify = replace(classify, p_yes > .4, "y")) %>% 
    mutate(classify = replace(classify, p_yes <= .4, "n"))

table(sw.news.predict$classify)
91+253
94+250
54+290
98+246 # gbm
47 + 297 # lm repeated cv

# Save into excel for checking on predicted docs
    ## xlsx::write.xlsx(sw.news.predict, "cr_data/sw_news_checkingprediction344_log.xlsx")
    
    ## xlsx::write.xlsx(sw.news.predict, "cr_data/sw_news_checkingprediction344_gbm.xlsx")
    
    # xlsx::write.xlsx(sw.news.predict, "cr_data/sw_news_checkingprediction344_lm_repeatcv.xlsx")


# MERGE BACK IN WITH THE MAIN DATAFRAME 
#########################################

glimpse(sw.news.predict)
sw.news.predict <- select(sw.news.predict, id, title,  classify)

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
354+990
398+946
347+997

sw.news.labelled_full <- sw.news.labelled_full %>% 
    select(-classify.x, -classify.y)


# FILTER OUT THOSE THAT ARE NOT RELEVANT/INCLUDING THOSE THAT ARE
# ##############################################################
    # I checked those that probability is from 30% to <50%
    # These are borderline that may be relevant but was excluded

# Include new 
include_ids <- c(281, 1100, 189, 581, 616, 724, 835, 989) # these needs N to be replaced with Y
NROW(include_ids)
table(sw.news.labelled_full$classify)

for (num in include_ids) {
    sw.news.labelled_full <- sw.news.labelled_full %>% 
            mutate(classify = replace(classify, id == num, "y"))
    }
table(sw.news.labelled_full$classify)
#View(sw.news.labelled_full[835,])

# Exclude 
exclude_ids <- c(63, 66, 194, 312, 355,386, 553, 642, 696, 719, 822, 925, 
                 975, 977, 1023, 1063, 1077, 1088,1119, 1125, 1153, 1156, 1160
                 ) # these needs N to be replaced with Y
NROW(exclude_ids) # 22
table(sw.news.labelled_full$classify)

for (num in exclude_ids) {
    print(num)
    sw.news.labelled_full <- sw.news.labelled_full %>% 
        mutate(classify = replace(classify, id == num, "n"))   
}

table(sw.news.labelled_full$classify)
365/1344 
979/1344 
janitor::tabyl(sw.news.labelled_full$classify)
# 27% was NO
# 73% was YES  

# REDUCE THE DATASET TO THOSE THAT ARE "Y"
###########################################
###########################################
sw.news.labelledYES <- sw.news.labelled_full %>% 
             filter(classify == "y")


# SAVE DATASET FOR ANALYSIS
############################
sw.news.labelledYES <- select(sw.news.labelledYES, 
                              -(classify),
                              -(log.phrase_pred),
                              -(sg_present_text),
                              -(sg_present_section),
                              -(asia_world_section))

an_swnews <- sw.news.labelledYES 
save(an_swnews, file = "an_data/an_swnews.RData") 





