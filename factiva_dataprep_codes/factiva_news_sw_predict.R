# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# After training the model in do-file "news_sw_training.R", i now predict on the rest of the documents.
# The model "final_trained_model"  was trained on 1000 documents "factiva_dataprep_codes/factiva_news_sw_training". 
# I predict using this model on the remaining documents

# Section A. Prediction [Dataset = sw.news.labelled]
    ## I load in the "swnews_withpreds.RData" which contains: 
    ## (1) entire dataset (2) with the predictor variables
    ## Subset of these dataset has been coded in the excel "sw_news_randomsample1000factiva_labelled.xlsx"
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
load(file = "cr_data/factiva/swnews_withpreds.RData")


# Check duplicates for title and text
#######################################

swnews_withpreds$title_duplicates <- duplicated(swnews_withpreds$title)
sum(swnews_withpreds$title_duplicates) # no duplicates

swnews_withpreds$text_duplicates <- duplicated(swnews_withpreds$text)
sum(swnews_withpreds$text_duplicates) # no duplicates
view.dup <- swnews_withpreds %>% filter(text_duplicates == T)
    # there are dups. So i do the below to show all the rows that are duplicates
dup <-
    duplicated(pull(swnews_withpreds,text)) |
    duplicated(pull(swnews_withpreds,text), fromLast=TRUE)

dup_text <-
    swnews_withpreds[dup, c('id', 'title', 'text')]

rm_dup <- c(1465, 1793, 4641,4937,5164,5222,5445,5525)

for (i in rm_dup) {
    swnews_withpreds <- swnews_withpreds %>% filter(id != i)
}

NROW(swnews_withpreds)
# 9869 -> 9861 


#######################
# SECTION A. PREDICTION
#######################

# PREDICT TRAINED MODEL ON REMAINING DATASET
#############################################


# Join in handcoded dataset with 1000 observation
# ===============================================

# Merge in the handcoded labels "classify"
library(readxl)
handcoded <- read_xlsx("cr_data/factiva/sw_news_randomsample1000factiva_labelled.xlsx", 
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
load(file = "cr_data/factiva/final_trained_model", verbose = T)
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
    mutate(classify = replace(classify, p_yes >= .70, "y")) %>% 
    mutate(classify = replace(classify, p_yes < .70, "n"))

table(sw.news.predict$classify)

1257 + 7605 # 8862

# Save into excel for checking on predicted docs
    #  xlsx::write.xlsx(sw.news.predict, "cr_data/factiva/sw_news_checkingprediction_lm_repeatcv_logpred_FACTIVA.xlsx")
    

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


1472+8389 # N = 9861


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
include_ids <- c(1808,4097,4578,4585,4664,7212,5246,5214,5669,6925) 
                # these needs N to be replaced with Y
                # These doc are below .70 threshold but >.60
                # but I manually checked that they should be in

sw.news.labelled_full[sw.news.labelled_full$id==6925]
                
NROW(include_ids)
table(sw.news.labelled_full$classify)

1472 + 8389 # N = 9861

for (num in include_ids) {
    sw.news.labelled_full <- sw.news.labelled_full %>% 
            mutate(classify = replace(classify, id == num, "y"))
} # Loop function: for those values in "include_ids", replace with Y

table(sw.news.labelled_full$classify)
1462 + 8399 # N = 9861

sw.news.labelled_full[sw.news.labelled_full$id==1808] # classified now as Y

# Exclude 
exclude_ids <- c(3440,1665,8560,345,346,866,6582,6597,6766,6769,7145,7149,7306,7373,7467,7944,8464,8565,8736,9506,9730,422,718,999,4662,4990,5014,5031,5048,5065,5147,5154,5261,5329,5386,5534,5616,5640,5657,5714,5717,5780,5864,5937,5978,5983,5986,6035,6071,6148,6156,6432,6451,6481,6490,6529,6604,6612,6615,6620,6626,6654,6658,6661,6725,6824,6836,6881,6885,7572,7608,7756,7764,7765,7796,7902,8012,8215,8229,8244,8260,8349,8428,8571,8674,8840,9312,9434,4461,989,8640,4663,6578,4718,5126,5785,6428,5602,6036,6401,7907,9048,1006,1803,4757,5318,5565,5718,5910,5911,6027,6048,6274,6344,6433,6484,6884,7439,8297,8546,6299,8247,10,17,21,40,43,44,45,61,92,102,107,135,145,155,162,163,169,181,194,199,212,272,327,419,424,512,545,570,589,620,663,750,849,883,985,1016,1017,1103,1130,1134,1135,1219,1334,1347,1432,1517,1567,1642,1670,1677,1709,1731,1748,1796,1800,1853,1855,2030,2058,2062,2071,2093,2094,2210,2215,2221,2259,2360,2368,2371,2379,2386,2395,2404,2409,2412,2413,2414,2415,2416,2417,2418,2420,2422,2427,2428,2431,2433,2436,2445,2447,2467,2579,2596,2600,2601,2604,2605,2635,2712,2737,2740,2752,2762,2764,2882,2991,2995,2998,3001,3019,3036,3069,3077,3090,3229,3324,3387,3443,3444,3457,3504,3511,3516,3526,3546,3568,3595,3609,3649,3689,3711,3713,3727,3759,3766,3783,3785,4146,4205,4224,4236,4269,4303,4340,4347,4349,4380,4455,4476,4486,4493,4502,4537,4571,4723,4762,4814,4818,4867,4874,4888,4962,4982,5050,5053,5100,5127,5160,5187,5257,5362,5593,5606,5665,5691,5881,5901,5917,5999,6015,6170,6188,6195,6206,6225,6233,6240,6241,6383,6540,6664,6700,7148,9010,9227,9712,9721,9804,462,1166,2557,2570,2649,2650,2651,2834,3228,3459,3969,4142,4178,4203,4244,4584,5447,5599,5700,5737,6224,6251,6253,6373,6939,8094,8938,8966,9592,1838,1861,4891,5036,5185,5199,5356,5471,5741,5916,6302,6441,6565,6610,6648,6719,6929,7031,7041,7175,7319,7327,7342,7519,7606,7643,7747,7754,7766,7897,7916,8498,8614,8687,9042,9217,9262,9437,9531,119,120,303,935,1010,1053,1140,1588,1740,2020,2031,2212,2224,2232,2234,2240,2243,2244,2314,2318,2704,2714,2773,2813,2827,2912,3107,3133,3287,3305,3311,3369,3374,3395,3429,3528,3554,3566,3593,3621,3624,3647,3913,3948,3973,3975,3986,3997,4029,4050,4066,4176,4267,4339,4610,5349,5698,5875,6046,6694,7621,7983,8716,8868,8984,9817,1123,1565,4365,8941,8652,2523,3681,3991,4204,3601,3602,3611,3613,3616,3620,3650,3652,4019,4056,4065,4334,6177,991,7345,7570,8510,9180,9339,993,2189,4987,5183,335,574,585,629,656,818,1033,1206,1400,1515,1868,2217,2235,4299,4515,5273,5607,5703,5888,6902,9284,9346,9504,9869,5582,884,2180,5548,6044,7400,1649,4111,4637,5478,6633,5074,5210,1582,486,905,481,1210,2532,2735,2736,4575,5058,6854,7052,9127,59,566,4025,4429,1118,5371,8963,4026,2703,9355,340,834,6323,2056) 
                    # These meets threshold of .70 and above
                    # But i manually check that they need to be excluded. 
                    # These articles that the model was not built to detect
                    # these needs N to be replaced with Y
                    # these articles are excluded from the corpuse because:
                        # (1) They are summary/hghlights of the news [e.g., Top headlines; "what's news"]
                        # (2) Not Singapore (e.g. news from HK, USA, UK, Vietnam, Malaysia etc)
                        # (3) Not social work - volunteering work
                        # (4) Social workers who are interviewed for non-work issues 
                        # [e.g., social worker who participated in an SDU event]
                        # (5) International or local Movies,talks, and dramas [often from Life section]
                        # (6) Advertisements/Weekend events 
                        # (7) Duplicates in texts
                        # (8) etc.
NROW(exclude_ids) # 551
table(sw.news.labelled_full$classify)
1462 + 8399 # N = 9861

for (num in exclude_ids) {
    print(num)
    sw.news.labelled_full <- sw.news.labelled_full %>% 
        mutate(classify = replace(classify, id == num, "n"))   
}

table(sw.news.labelled_full$classify)

2013+7848 # 9861

janitor::tabyl(sw.news.labelled_full$classify)
# 20% was NO
# 80% was YES  

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
                              -(text_duplicates),
                              -(year),
                              -(file_id),
                              -(id_factiva))
        # i do not need the predictors in the dataset.                    

an_swnews <- sw.news.labelledYES  # save as analytical dataset
save(an_swnews, file = "an_data/factiva/an_swnews.RData") 

rm(list=ls())



