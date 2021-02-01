# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# After training the model in do-file "news_sw_training.R", i now predict on the rest of the documents.
# The model "final_trained_model" in cr_data folder was trained on 1000 documents. 
# I predict using this model on the remaining documents

# Section A. Prediction [Dataset = sw.news.labelled]
## I load in the "swnews_withpreds.RData" which contains: 
## (1) entire dataset (2) with the predictor variables
## Subset of these dataset has been coded in the excel "sw_news_randomsample1000lexis_labelled.xlsx"
## Hence I load in this excel and merge in with the R dataset 
## Later on, I subset to those rows that are not labelled for prediction using the final_trained_model 

# Section B. Threshold and classifying [Dataset = sw.news.predict]
## The prediction using the model will produce the probabilities for each document
## Then i will set a threshold to classify the docs
## After all have been classified, i then have to merge in back with the full dataframe with all the vars
## i.e., "sw.news.predict merged" in back with "sw.news.labelled"

# SECTION C. CLEANING AND PREP FINAL DATASET
## We will do a check on the dataset and remove or include articles 
## And then filter out those that were classified as N

rm(list = ls())

library(textreadr)
library(stringr)
library(dplyr)
getwd()

# load RData dataset
####################
load(file = "cr_data/swnews_withpreds.RData")


# Check duplicates for title and text
#######################################

swnews_withpreds$title_duplicates <- duplicated(swnews_withpreds$title)
sum(swnews_withpreds$title_duplicates) # no duplicates

swnews_withpreds$text_duplicates <- duplicated(swnews_withpreds$text)
sum(swnews_withpreds$text_duplicates) # no duplicates

#######################
# SECTION A. PREDICTION
#######################

# PREDICT TRAINED MODEL ON REMAINING DATASET
#############################################


# Join in handcoded dataset with 1000 observation
# ===============================================

# Merge in the handcoded labels "classify"
library(readxl)
handcoded <- read_xlsx("cr_data/sw_news_randomsample1000lexis_labelled.xlsx", 
                       col_names = T)
            # this is the excel with 1000 articles labelled manually

tail(handcoded, n=5) # check last five rows
table(handcoded$classify) 

handcoded <- handcoded %>% select(id, title, classify) # keep 3 vars 

sw.news.labelled <- left_join(swnews_withpreds, handcoded, by = c("id", "title"))
    # merge in with the entire dataset.
    # The classify with N and Y are those labelled manually. The NA will be the ones
    # I need to predict using the train model
    
glimpse(sw.news.labelled)
table(sw.news.labelled$classify, exclude = NULL)

view.sw.news.labelled <- sw.news.labelled %>% 
    select(id, title, classify)


# Load in final.train.model 
# ==========================
    # load in final.train.model to pred on remaining 344 docs
load(file = "cr_data/final_trained_model", verbose = T)
    # this is the object saved in "do-file news_sw_training.R"

# Predicting to create the probabilities for each classification
sw.news.predict <- sw.news.labelled %>% 
    select(id, title, text, ends_with("_pred"), classify) %>% 
    filter(is.na(classify))
    # keep only those rows (6000+) that have not been labelled

p <- predict(final_trained_model, sw.news.predict, type = "prob")
    # if type = "raw", predict will auto use .50 threshold
head(p, n=10)
class(p)

#p_raw <- predict(final.train.model, sw.news.predict, type = "raw")
#p_options <- tibble(p_raw, p)
#View(p_options)

# Merge in the variable into sw.news.predict
    # p created by predict command is a dataframe with n and y variables
    # so i bring p$y into sw.news.predict as a new var
sw.news.predict$p_yes <- p$y
summary(sw.news.predict$p_yes)

######################################################
# Section B. Threshold and classifying
######################################################

# Using threshold to classify the variable
sw.news.predict <- sw.news.predict %>% 
    mutate(classify = replace(classify, p_yes > .5, "y")) %>% 
    mutate(classify = replace(classify, p_yes <= .5, "n"))

table(sw.news.predict$classify)

47 + 297 # old
1382+4569 # new

# Save into excel for checking on predicted docs
    #  xlsx::write.xlsx(sw.news.predict, "cr_data/sw_news_checkingprediction_lm_repeatcv_LEXIS.xlsx")
    

# MERGE BACK IN WITH THE MAIN DATAFRAME 
#########################################

glimpse(sw.news.predict)
sw.news.predict <- select(sw.news.predict, id, title,  classify)

table(sw.news.labelled$classify, exclude = F)

sw.news.labelled_full <- left_join(sw.news.labelled, sw.news.predict, by = c("id", "title"))
    # the join produce two vars of classify that complements
table(sw.news.labelled_full$classify.x) 
table(sw.news.labelled_full$classify.y) 

sw.news.labelled_full <- sw.news.labelled_full %>% 
 mutate(classify = coalesce(classify.x, classify.y)) 
    # combine both vars
    
table(sw.news.labelled_full$classify)
table(sw.news.labelled_full$classify.x)
table(sw.news.labelled_full$classify.y)
391+953
394 + 950
354+990
398+946
347+997

# 1719+5232
# 5232/6951


################################################
# SECTION C. CLEANING AND PREP FINAL DATASET
###############################################

sw.news.labelled_full <- sw.news.labelled_full %>% 
    select(-classify.x, -classify.y)


# FILTER OUT THOSE THAT ARE NOT RELEVANT/INCLUDING THOSE THAT ARE
# ##############################################################
    # I checked those that probability is from 30% to <50%
    # These are borderline that may be relevant but was excluded


# Include new 
include_ids <- c(761,897,1428,1823,2047,2434,3052,3257,3444,3573,3603,
                  3837,4347,4609,4668,4746,5012,5051,
                  5680,6472) 
                # these needs N to be replaced with Y
                # These doc are below .50 threshold but I manually checked that they should be in
                
NROW(include_ids)
table(sw.news.labelled_full$classify)
# 1719+5232
# 5232/6951

for (num in include_ids) {
    sw.news.labelled_full <- sw.news.labelled_full %>% 
            mutate(classify = replace(classify, id == num, "y"))
} # Loop function: for those values in "include_ids", replace with Y

table(sw.news.labelled_full$classify)
# 1699+5252
5252/6951 

view.included <- sw.news.labelled_full %>% select(id, title, classify)

#View(sw.news.labelled_full[835,])

# Exclude 
exclude_ids <- c(964,2145,2460,2942, 3087, 3805,3855,3996, 4238, 4685, 4728,
                 4883,4910, 5133,5230,5234, 5644, 6038,6171, 6185, 6340, 6724, 
                 6923, 5328, 5385,6641, 471, 1092,867,1415, 1458, 1470,1550,1600,
                 1604, 1617, 1660, 1690, 1708, 1781,1792, 1856,2035, 2088, 2188,
                 2189,2369, 2375, 6922, 6937,6946, 6949, 5610, 569, 675, 872, 2387,
                 2393, 2397, 2358, 3691,3693, 5787, 799, 7060, 1795, 2196, 3725, 
                 3735, 3736, 3758, 3768, 3775, 3851, 3888, 2391, 3600, 3554,3070,
                 3012,648, 5885, 2196, 2091, 3692,973, 4840) 
                 
                    # these needs N to be replaced with Y
                    # these articles are not useful to the corpus:
                        # (1) They are summary/hghlights of the news
                        # (2) Not Singapore
                        # (3) Not social work - vol work
                        # (4) Social workers who are interviewed for non-work issues
                        # (5) etc.
NROW(exclude_ids) # 22
table(sw.news.labelled_full$classify)
1699+5252
5252/6951

for (num in exclude_ids) {
    print(num)
    sw.news.labelled_full <- sw.news.labelled_full %>% 
        mutate(classify = replace(classify, id == num, "n"))   
}

table(sw.news.labelled_full$classify)
365/1344 
979/1344 
1783 + 5168
5168/6951 # 74%
janitor::tabyl(sw.news.labelled_full$classify)
# 26% was NO
# 74% was YES  

# REDUCE THE DATASET TO THOSE THAT ARE "Y"
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
                              -(asia_world_section),
                              -(title_duplicates),
                              -(text_duplicates))
        # i do not need the predictors in the dataset.                    

an_swnews <- sw.news.labelledYES  # save as analytical dataset
save(an_swnews, file = "an_data/an_swnews.RData") 





