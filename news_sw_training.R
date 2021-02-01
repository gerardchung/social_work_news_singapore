########################################
# TEXT-MING SOCIAL WORK NEWS 
########################################
## Section A 
# I will first prepare the predictors for the training model to dev a classficiation model

## Section B 
  # Then, I need to check for duplicates in titles of documents.
  # Remove also year 2021

## Section C
# To randomly sample 1000 documents to develop a classification model
# This will training set will be saved in an excel and manually labelled
# Then it will be joinned in (merge) and training models will be used
# What models: (1) Logistic reg; (2) GBM (3) RF
# Resampling will used: 10-fold CV (repeated 5)
# Cross-validation = 1000; Remaining 6000+ will be predicted


rm(list = ls())

library(textreadr)
library(stringr)
library(dplyr)
library(caret)
getwd()

# load RData dataset
####################
load(file = "cr_data/sw_news.RData")

# Convert foldernum to numeric
sw.news$foldernum  <- as.numeric(sw.news$foldernum)

# create id 
sw.news <- mutate(sw.news, id = row_number()) %>% 
  relocate(id, .before = foldernum)

#############
## Section A
############# 

# CLEAN VARIABLES & CREATE PREDICTORS
######################################
    # keep original variables title and foldernum so that i can merge in later

# title 
# =====
    # does Social work appear in title? (yes/no)
#str_view(sw.news$title, pattern = "[sS]ocial [wW]ork")
sw.news <- sw.news %>% 
    mutate(title_pred = str_detect(title, pattern = "[sS]ocial [wW]ork")) 
            # see below to do case insensitivity


sw.news$title_pred <- case_when(sw.news$title_pred == T ~ 1, 
                                sw.news$title_pred == F ~ 0)

title_pred_label = c("No phrase", "Has phrase")
sw.news$title_pred <- factor(sw.news$title_pred, labels = title_pred_label)
table(sw.news$title_pred, exclude = NULL)


# text
# ====
    # Num of times phrase appears
# str_view_all(sw.news$text[84],  regex(pattern = "social-work|social work", ignore_case = T))


sw.news <- sw.news %>% 
    mutate(phrase_pred = str_count(text, regex(pattern = "social-work|social work", ignore_case = T)))
summary(sw.news$phrase_pred)

#require(ggplot2)
#ggplot(sw.news, aes(x = phrase_pred) ) + geom_bar()

View.phrase_pred <- sw.news %>% 
    filter(phrase_pred > 18)

sw.news <- sw.news %>% 
  mutate(log.phrase_pred = log(phrase_pred + .0001)) %>% 
  relocate(log.phrase_pred, .after = phrase_pred)
summary(sw.news$log.phrase_pred)

# section
# ========
unique(sw.news$section[1:10])

# Geographic
# =========
    # this predictor will have three conditions: In Singapore, not in Singapore, Not sure
unique(sw.news$geographic[1:20]) # there are NA

#str_view(sw.news$geographic, regex(pattern = "singapore", ignore_case = T))

sw.news <- sw.news %>% 
    mutate(geographic_pred = str_detect(geographic, regex(pattern = "singapore", ignore_case = T)))

table(sw.news$geographic_pred , exclude = NULL) 
    # missing values because there are missing for geographic
    # i will fill in the NA using two conditions below

# Replace missing values in geographic_pred conditional on the following two conditions:
# In text: the word Singapore appears
# In section: the word Home or Singapore appears (Home => "Home news" Singapre News")
# If still missing, then fill value "not sure"

section_pattern <- "home|singapore"
section_pattern_negate <- "asia|world|malaysia"  # news in the asia/world section will be exclude i.e. False

#str_view_all(sw.news$section, regex(pattern = section_pattern_negate, ignore_case = T))

sw.news <- sw.news %>% 
 mutate(
   sg_present_text    = str_detect(text, regex(pattern = "singapore", ignore_case = T)),
   sg_present_section = str_detect(section, regex(pattern = section_pattern, ignore_case = T)),
  asia_world_section = str_detect(section, regex(pattern = section_pattern_negate, ignore_case = T))
    )
    
view.phrase <-  select(sw.news, text, sg_present_text, section, 
                  sg_present_section, asia_world_section, geographic, geographic_pred )


sw.news <- sw.news %>% 
        mutate(geographic_pred = replace(geographic_pred, 
                                         sg_present_text == T | sg_present_section == T, T),
               geographic_pred = replace(geographic_pred, 
                                         asia_world_section == T , F)
               )

# the abv command replace geographic_pred with TRUE if it meets the both conditions
# But there is still missing 

table(sw.news$geographic_pred, exclude = NULL) 

sw.news$geographic_pred <- case_when(sw.news$geographic_pred == T ~ "SG",
                                     sw.news$geographic_pred == F ~ "Not SG",
                               is.na(sw.news$geographic_pred) ~ "unsure")

table(sw.news$geographic_pred, exclude = NULL) 

sw.news$geographic_pred <- factor(sw.news$geographic_pred)
table(sw.news$geographic_pred)                             


# key words in text
# =================
    # keywords_pred = Did key words appeared? (yes/no)
    # key words = "social service", "social-service", "ministry", "ncss", "vwo", "sasw" "fsc"

pattern_keywords <- "social service|social-service|vwo|voluntary welfare organization|family service centre|fsc|ncss|singapore association of social workers|sasw|s r nathan|ann wee"

#str_view(sw.news_sample$text[80], regex(pattern = pattern_keywords, ignore_case = T))

sw.news  <- sw.news %>% 
        mutate(keywords_pred = str_detect(text, 
                                          regex(pattern = pattern_keywords, ignore_case = T)))

sw.news$keywords_pred <- case_when(sw.news$keywords_pred == T ~ 1, 
                                   sw.news$keywords_pred == F ~ 0)

view.keyword <- select(sw.news, text, id , keywords_pred)
table(sw.news$keywords_pred, exclude = NULL)

sw.news$keywords_pred <- factor(sw.news$keywords_pred, labels = c("no", "yes"))
table(sw.news$keywords_pred, exclude = NULL)

# subject
# ========
      # Did list of subject appeared? (yes/no)
head(sw.news$subject, n=100)

pattern_subject = "family services|abuse|family|children|youth|mental health|domestic violence|welfare|poverty|low income|elderly|senior citizen|criminal|crime|corrections|
nursing home|counsel|marriage|neglect|university"

#str_view(sw.news$subject, regex(pattern = pattern_subject, ignore_case = T))
sw.news <- sw.news %>% 
    mutate(subject_pred = str_detect(subject, 
                                     regex(pattern = pattern_subject, ignore_case = T)))

sw.news$subject_pred <- case_when(sw.news$subject_pred == T ~ 1,
                                  sw.news$subject_pred == F ~ 0,
                                  is.na(sw.news$subject_pred)  ~ 0)

sw.news$subject_pred <- factor(sw.news$subject_pred, labels = c("no", "yes"))

table(sw.news$subject_pred , exclude = NULL)


# Source
########

unique(sw.news$source)

# Today
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source =="Today (Singapore) - Online" , "Today")
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source ==("TODAY (Singapore)") , "Today")

# ST
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source =="The Straits Times (Singapore)" , "Straits Times")
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source =="The Straits Times" , "Straits Times")

# BT
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source =="The Business Times Singapore" , "Business Times")
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source =="Business Times (Singapore)" , "Business Times")

# CNA
sw.news$source_pred <-  replace(sw.news$source_pred, sw.news$source =="Channel NewsAsia" , "Channel NewsAsia")


unique(sw.news$source)
unique(sw.news$source_pred)

sw.news$source_pred <- factor(sw.news$source_pred)

table(sw.news$source_pred, exclude = F)
table(sw.news$source, exclude = F)


#############
## Section B
############# 

# FILTER OUT THOSE THAT ARE IN 2021 YEAR
#########################################
swnews_withpreds <- sw.news

# Check for year 2021
swnews_withpreds$year <-  str_extract(swnews_withpreds$pub.date, pattern = "\\d{4}")
table(swnews_withpreds$year)

# Check for duplicates in titles
swnews_withpreds$title_duplicates <- duplicated(swnews_withpreds$title)

View.duplicates <- swnews_withpreds %>% 
  select(title, text, pub.date, title_duplicates) %>% 
  filter(title_duplicates == T)
# View(View.duplicates) # these obs are duplicates but will not include the original ones

# Remove both year 2021 and duplicates in titles
swnews_withpreds <- swnews_withpreds %>% 
    filter(year != "2021") %>%  # removre year 2021
    filter(title_duplicates != T)  %>%  # remove duplicates in titles
    select(-(year), -(title_duplicates)) # remove these two variables

sum(duplicated(swnews_withpreds$title)) # check again if have duplicates

nrow(swnews_withpreds) # N = 6951 (after removing year 2021 and duplicates in titles)


#############
## Section C
############# 
  
# SAVE DATAFRAME
#################
  # this dataframe will be reloaded in prediction do-file for predicting the remaining documents
save(swnews_withpreds, file = "cr_data/swnews_withpreds.RData")


# Sample 1000 txts
####################

#set.seed(345)
set.seed(346)

# shuffles the rows
rows <- sample(nrow(swnews_withpreds))
sw.news_shuffled <- swnews_withpreds[rows,]

sw.news_sample <- sample_n(sw.news_shuffled, size = 1000, replace = F) # 300 of 1344

View.randomsample <- sw.news_sample %>% 
  select(1:2)

table(sw.news_sample$foldernum)

getwd()
save(sw.news_sample, file = "cr_data/sw_news_randomsample1000lexis.RData") 
#old: save(sw.news_sample, file = "cr_data/sw_news_randomsample1000.RData") 

sw.news_sample_randomsample1000 <- sw.news_sample %>% 
  select(-ends_with("_pred"), -ends_with("_section"), -ends_with("_text"))

#xlsx::write.xlsx(sw.news_sample_randomsample1000, "cr_data/sw_news_randomsample1000lexis.xlsx")
  # dont run this command again if you have coded the same file; it will rewrite it

# old: xlsx::write.xlsx(sw.news_sample_randomsample1000, "cr_data/sw_news_randomsample1000.xlsx")

# REDUCE DATASET
# ==============
sw.news_sample_reduced <- sw.news_sample %>% 
    select(1:3, ends_with("pred"))

summary(sw.news_sample_reduced) # check no NA

View.sw.news_sample_reduced <- sw.news_sample_reduced %>% 
  select(1:2, title)

# Remove objects from Memory
rm(View.duplicates)
rm(View.phrase_pred, View.randomsample, View.sw.news_sample_reduced, view.keyword, view.phrase)
rm(sw.news_sample, sw.news_sample_randomsample1000, sw.news_shuffled)

# Create test and train sets
#############################

# Merge in the handcoded labels "class"
library(readxl)
handcoded <- read_xlsx("cr_data/sw_news_randomsample1000lexis_labelled.xlsx", 
                       col_names = T, 
                       )

glimpse(handcoded)

tail(handcoded, n=5) # check last five rows
tail(sw.news_sample_reduced, n=5) # check last five rows

table(handcoded$classify) # no information rate = 66% 
663/1000 # no information rate = 66% 

handcoded <- handcoded %>% select(3,title, classify) # keep 3 vars 

# old:validation <- inner_join(sw.news_sample_reduced, handcoded, by = c("id", "title"))

validation <- inner_join(sw.news_sample_reduced, handcoded, by = c("id", "title")) 

view.validation <- validation %>% select(id, title, classify)

summary(validation)
tail(validation, n = 5)
table(validation$classify, exclude = NULL)

head(validation$classify)
str(validation$classify)

#validation$classify <- if_else(validation$classify == "y", 1, 0)
validation$classify <- factor(validation$classify, labels = c("n","y"))
    # this will convert to factor with 1 and 2 
    # 1/2 values for outcome does not matter in R for logisic reg
    # converting to factor is helpful because confusionMatrix requires factors
    
# I DO NOT NEED TRAINING AND TEST BECAUSE I DOING CV ON ENTIRE VALIDATION
# Create training set
set.seed(345)
train <- validation %>% slice_sample( prop = .80)
# Create test set
test  <- anti_join(validation, train, by = 'id')

summary(validation)


# Caret logistic regression
############################
library(caret)


# Create formula 
# ===============

# model 1
predictors1 <- c("phrase_pred", "title_pred", "geographic_pred", "subject_pred", "keywords_pred", "source_pred")
formula1 <- formula(paste("classify ~", 
                    paste(predictors1, collapse=" + "))
                    ) 
formula1

# model log.phrase_pred
predictors_log <- c("log.phrase_pred", "title_pred", "geographic_pred", "subject_pred", "keywords_pred", "source_pred")
formula_log <- formula(paste("classify ~", 
                          paste(predictors_log, collapse=" + "))
) 
formula_log


# model 2 interaction1
predictors_interact <- c("phrase_pred", "title_pred", 
                         "geographic_pred", "subject_pred", "keywords_pred", "source_pred",
                         "phrase_pred*keywords_pred")
formula_interact <- formula(paste("classify ~", 
                          paste(predictors_interact, collapse=" + "))
                   ) 
formula_interact


# Model 1: formula1 (no interaction)
# ====================================
sw_news_glm <- glm(formula =  formula1 ,
                     data = train ,
                     family = "binomial")


sw_news_glm
summary(sw_news_glm)
exp(coef(sw_news_glm))
format(exp(coef(sw_news_glm)), scientific = F, digits = 3)

p <- predict(sw_news_glm, test, type = "response")



# ROC
# ====
library(caTools)
colAUC(p, test$classify, plotROC = T)
# calculates AUC = .835

thresh <- .5

y_or_n <- ifelse(p > thresh, "y", "n")

#table1 <- table(y_or_n, test$classify)

# table_lowthreshold  <- table(y_or_n, test$classify)
# table_highthreshold <- table(y_or_n, test$classify)
# table_50hreshold <- table(y_or_n, test$classify)

# confusionMatrix(table_lowthreshold , positive = "y")
# confusionMatrix(table_highthreshold, positive = "y")
# confusionMatrix(table_50hreshold, positive = "y")


# confusionMatrix(table1)
confusionMatrix(as.factor(y_or_n), as.factor(test$classify), positive = "y")
    # confusionMatrix requires both to be factors or in table 
    # this is better becos the output will show which is prediction and reference
    # NIR no info rate: if we do not know anything, our best guess is to choose the majority class



# Model 2: formula_interaction (interaction)
# ====================================
sw_news_glm2 <- glm(formula =  formula_interact,
                   data = train ,
                   family = "binomial")


sw_news_glm2
summary(sw_news_glm2)
exp(coef(sw_news_glm2))

p2 <- predict(sw_news_glm2, test, type = "response")

# ROC
# ====
colAUC(p2, test$classify, plotROC = T)
# calculates AUC = .836 -> not difference from model1   

thresh <- .5

y_or_n2 <- ifelse(p2 > thresh, "y", "n")
confusionMatrix(as.factor(y_or_n2), as.factor(test$classify), positive = "y")

confusionMatrix(as.factor(y_or_n2), as.factor(test$classify))


# Cross-validation with/without repeat
##############################
formula1

# CV settings
# ============
    # use validation set (n=1000)
    # 10 fold 
    # if with repeat, repeated = 5


myControl <- trainControl(
    method = "cv",
    number = 10 ,
    classProbs = T,
    summaryFunction = twoClassSummary, # if you remove this arg, you can get accuracy for model_lm
    verboseIter = T
)

  # summaryFunction = twoClassSummary give ROC
  # remove it and it will give you accuracy


myControl_repeatedcv <- trainControl(
    method = "repeatedcv",
    number = 10 ,
    repeats = 5, 
    classProbs = T,
    summaryFunction = twoClassSummary,
    verboseIter = T
)

# GLM model
# ========

# CV GLM
set.seed(222)
model_lm <- train(
    form = formula1, 
    data = validation,
    method = "glm",
    trControl = myControl
)

model_lm #ROC = .823
model_lm$finalModel 
exp(model_lm$finalModel$coefficients) # odds ratio 

model_lm$resample
sd(model_lm$resample$Accuracy)

model_lm$results  # accuracy, kappa, accuracySD, kappaSD
                  # accuracy = average across the ten [model_lm$resample]
                  # ROC = .823, SD = .04
                



# CV repeated GLM
set.seed(222)
model_lm_repeatecv <- train(
                        form = formula1, 
                        data = validation,
                        method = "glm",
                        trControl = myControl_repeatedcv
)

model_lm_repeatecv # ROC = .83 
model_lm_repeatecv$finalModel 
exp(model_lm_repeatecv$finalModel$coefficients) # odds ratio 

model_lm_repeatecv$resample
sd(model_lm_repeatecv$resample$Accuracy)

model_lm_repeatecv$results # ROC = .83, SD = .04



# CV repeated GLM log.phrase_pred
set.seed(222)
model_lm_repeatecv_log <- train(
  form = formula_log, 
  data = validation,
  method = "glm",
  trControl = myControl_repeatedcv
)

model_lm_repeatecv_log #  ; ROC = .826
model_lm_repeatecv_log$finalModel 
exp(model_lm_repeatecv_log$finalModel$coefficients) # odds ratio 

model_lm_repeatecv_log$resample
sd(model_lm_repeatecv_log$resample$Accuracy)

model_lm_repeatecv_log$results # ROC = .826, SD = .035


# GBM model (Stochastic Gradient Boosting)
# =======================================
set.seed(222)
model_gbm <- train(
    form = formula1, 
    data = validation,
    method = "gbm",
    trControl = myControl_repeatedcv
)

model_gbm 
model_gbm$finalModel 
#exp(model_gbm$finalModel$coefficients) # odds ratio 

model_gbm$resample 
sd(model_gbm$resample$Accuracy)

model_gbm$results


# GBM model with log pred
# =======================================
set.seed(222)
model_gbm_logpred <- train(
  form = formula_log, 
  data = validation,
  method = "gbm",
  trControl = myControl_repeatedcv
)

model_gbm_logpred # Accuracy = .79 do we use this or below command?
model_gbm_logpred$finalModel 
#exp(model_gbm$finalModel$coefficients) # odds ratio 

model_gbm_logpred$resample 
sd(model_gbm_logpred$resample$Accuracy)

model_gbm_logpred$results



# RF
# =======================================
set.seed(222)
model_rf <- train(
    form = formula1, 
    data = validation,
    method = "ranger",
    trControl = myControl_repeatedcv
)

model_rf # ROC = .80
model_rf$finalModel 
#exp(model_gbm$finalModel$coefficients) # odds ratio 

model_rf$resample
sd(model_rf$resample$Accuracy)

model_rf$results


# COMPARE MODELS
################
resamps <- resamples(list(
                   lm_repeatcv = model_lm_repeatecv,
                   lm_repeatcv_logpred = model_lm_repeatecv_log,
                   gbm = model_gbm,
                   gbm_logpred = model_gbm_logpred, 
                   rf = model_rf)
                   )

summary(resamps)

modelDifferences <- diff(resamps)
summary(modelDifferences)

?xyplot.resamples
dotplot(resamps,
        scales =list(x = list(relation = "free")),
        between = list(x = 2))

#bwplot(resamps,
#       metric = "ROC")
#
#splom(resamps, variables = "metrics")
#parallelplot(resamps, metric = "ROC")



# CHOSEN MODEL
##############

#save.image(file = "cr_data/news_sw_predict.RData")
#
#rm(list = ls())
#load("cr_data/news_sw_predict.RData")


final_trained_model <- model_lm_repeatecv #model_gbm 
      #model_lm_repeatecv_log   # GLM with repeated CV   model_lm_repeatecv
getwd()
save(file = "cr_data/final_trained_model", final_trained_model)


# Confusion matrix

confusionMatrix(model_lm_repeatecv, norm = "overall") # default is average across in percentage 
confusionMatrix(model_lm_repeatecv, norm = "none") # aggegrate across

# ROC with cross-valid
library(pROC)
myRoc <- roc(predictor = model_gbm$pred$versicolor, response = model$pred$obs, positive = 'versicolor')
plot(myRoc)

#There are several ways to show the table entries. Using norm = "none" will show the  #aggregated counts of samples on each of the cells (across all resamples). For norm =  #"average", the average number of cell counts across resamples is computed (this can help  #evaluate how many holdout samples there were on average). The default is norm = "overall",  #which is equivalento to "average" but in percentages.
  





