########################################
# TEXT-MING SOCIAL WORK NEWS 
########################################

## Section A
  # load dataset
  # This data already has predictors created in factiva_cleanvars.R dofile
  # I will create a new ID var that is for this dataset (n=9869). 
  # id_factiva is id var for entire dataset (10000+) before removal of duplicates and year =2021
  # 
## Section B 
  # Then, I need to check for duplicates in titles of documents.
  # Remove also year 2021

## Section C
# To randomly sample 1000 documents to develop a classification model
# This will training set will be saved in an excel and manually labelled
# Then it will be joinned in (merge) and training models will be used
# What models: (1) Logistic reg; (2) GBM (3) RF
# Resampling will used: 10-fold CV (repeated 5)
# Cross-validation = 1000; Remaining documents  will be predicted


rm(list = ls())

library(textreadr)
library(stringr)
library(dplyr)
library(caret)
getwd()

#############
## Section A
############# 

# load RData dataset
# ===================
load(file = "cr_data/factiva/sw.news.RData") # n = 9869

# create id 
# ===================
sw.news <- mutate(sw.news, id = row_number()) %>% 
  relocate(id, .before = file_id)

table(sw.news$id)


#################################################################
## Section B: check again that all dups and year 29021 removed
################################################################# 


# Check if year == 2021 is in dataset
# ======================================
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

nrow(swnews_withpreds) # N = 9869


#############
## Section C
############# 
  
# SAVE DATAFRAME
# ======================================

  # this dataframe will be reloaded in prediction do-file for predicting the remaining documents
save(swnews_withpreds, file = "cr_data/factiva/swnews_withpreds.RData")


# Sample 1000 txts
# ======================================


set.seed(111)

# shuffles the rows
rows <- sample(nrow(swnews_withpreds))
sw.news_shuffled <- swnews_withpreds[rows,]

sw.news_sample <- sample_n(sw.news_shuffled, size = 1000, replace = F) 

getwd()
save(sw.news_sample, file = "cr_data/factiva/sw_news_randomsample1000factiva.RData") 

sw.news_sample_randomsample1000 <- sw.news_sample %>% 
  select(-ends_with("_pred"), -ends_with("_section"), -ends_with("_text"))

#xlsx::write.xlsx(sw.news_sample_randomsample1000, "cr_data/factiva/sw_news_randomsample1000factiva.xlsx")
  # dont run this command again if you have coded the same file; it will rewrite it


# REDUCE DATASET
# ==============
sw.news_sample_reduced <- sw.news_sample %>% 
    select(id, file_id,id_factiva, title,  ends_with("pred"))

summary(sw.news_sample_reduced) # check no NA

View.sw.news_sample_reduced <- sw.news_sample_reduced %>% 
  select(1:2, title)

# Remove objects from Memory
rm(View.duplicates)
rm(sw.news_sample, sw.news_sample_randomsample1000, sw.news_shuffled)


# Create test and train sets
# ======================================


# Merge in the handcoded labels "class"
library(readxl)
handcoded <- read_xlsx("cr_data/factiva/sw_news_randomsample1000factiva_labelled.xlsx", 
                       col_names = T, 
                       )

glimpse(handcoded)

tail(handcoded$id, n=5) # check last five rows
tail(sw.news_sample_reduced$id, n=5) # check last five rows

table(handcoded$classify) # no information rate = 76% 
785/1000 # no information rate = 76% 

handcoded <- handcoded %>% select(id,title, classify) # keep 3 vars 

# old:validation <- inner_join(sw.news_sample_reduced, handcoded, by = c("id", "title"))

validation <- inner_join(sw.news_sample_reduced, handcoded, by = c("id", "title")) 

table(validation$classify, exclude = NULL)
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
# ======================================

library(caret)


# Create formula 
# ===============

# model 1
predictors1 <- c("phrase_pred", "title_pred", "geographic_pred", "subject_pred", "keywords_pred", "source_pred", "section_pred")
formula1 <- formula(paste("classify ~", 
                    paste(predictors1, collapse=" + "))
                    ) 
formula1

# model log.phrase_pred
predictors_log <- c("log.phrase_pred", "title_pred", "geographic_pred", "subject_pred", "keywords_pred", "source_pred", "section_pred")
formula_log <- formula(paste("classify ~", 
                          paste(predictors_log, collapse=" + "))
) 
formula_log


# model 2 interaction1
predictors_interact <- c("phrase_pred", "title_pred", 
                         "geographic_pred", "subject_pred", "keywords_pred", "source_pred", "section_pred",
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

summary(model_lm)
model_lm #ROC = .84
model_lm$finalModel 
exp(model_lm$finalModel$coefficients) # odds ratio 

model_lm$resample
sd(model_lm$resample$Accuracy)

model_lm$results  # accuracy, kappa, accuracySD, kappaSD
                  # accuracy = average across the ten [model_lm$resample]
                  # ROC = .84, SD = .059
                



# CV repeated GLM
set.seed(222)
model_lm_repeatecv <- train(
                        form = formula1, 
                        data = validation,
                        method = "glm",
                        trControl = myControl_repeatedcv
)

summary(model_lm_repeatecv)
model_lm_repeatecv # ROC = .839 
model_lm_repeatecv$finalModel 
exp(model_lm_repeatecv$finalModel$coefficients) # odds ratio 

model_lm_repeatecv$resample
sd(model_lm_repeatecv$resample$Accuracy)

model_lm_repeatecv$results # ROC = .839, SD = .047



# CV repeated GLM log.phrase_pred
set.seed(222)
model_lm_repeatecv_log <- train(
  form = formula_log, 
  data = validation,
  method = "glm",
  trControl = myControl_repeatedcv
)

summary(model_lm_repeatecv_log)
model_lm_repeatecv_log #  ; ROC = .837
model_lm_repeatecv_log$finalModel 
exp(model_lm_repeatecv_log$finalModel$coefficients) # odds ratio 

model_lm_repeatecv_log$resample
sd(model_lm_repeatecv_log$resample$Accuracy)

model_lm_repeatecv_log$results # ROC = .837, SD = .050


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

#save.image(file = "cr_data/factiva/news_sw_predict.RData")
#
#rm(list = ls())
#load("cr_data/news_sw_predict.RData")


final_trained_model <- model_lm_repeatecv_log #model_lm_repeatecv #model_gbm 
      #model_lm_repeatecv_log   # GLM with repeated CV   model_lm_repeatecv
getwd()
save(file = "cr_data/factiva/final_trained_model", final_trained_model)


# Confusion matrix

confusionMatrix(model_lm_repeatecv_log, norm = "overall") # default is average across in percentage 
confusionMatrix(model_lm_repeatecv_log, norm = "none") # aggegrate across

# ROC with cross-valid
library(pROC)
myRoc <- roc(predictor = model_gbm$pred$versicolor, response = model$pred$obs, positive = 'versicolor')
plot(myRoc)
plot(myRoc)

#There are several ways to show the table entries. Using norm = "none" will show the  #aggregated counts of samples on each of the cells (across all resamples). For norm =  #"average", the average number of cell counts across resamples is computed (this can help  #evaluate how many holdout samples there were on average). The default is norm = "overall",  #which is equivalento to "average" but in percentages.
  





