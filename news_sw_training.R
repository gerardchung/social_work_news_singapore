# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# To randomly sample 1000 documents to develop a classification model
# Cross-validation = 1000; Remaining 344 will be predicted


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
sw.news <- mutate(sw.news, id = row_number()) %>% 
  relocate(id, .before = foldernum)

# Sample 300 txts
####################
set.seed(345)

# shuffles the rows
rows <- sample(nrow(sw.news))
sw.news_shuffled <- sw.news[rows,]

sw.news_sample <- sample_n(sw.news_shuffled, size = 1000, replace = F) # 1000 of 1344
View_randomsample <- sw.news_sample %>% 
    select(1:2)
table(sw.news_sample$foldernum)

getwd()
save(sw.news_sample, file = "cr_data/sw_news_randomsample1000.RData") 
# xlsx::write.xlsx(sw.news_sample, "cr_data/sw_news_randomsample1000.xlsx")
    # dont run this command again if you have coded the same file; it will rewrite it


# CLEAN VARIABLES & CREATE PREDICTORS
######################################
    # keep original variables title and foldernum so that i can merge in later

# title 
# =====
    # does Social work appear in title? (yes/no)
str_view(sw.news_sample$title, pattern = "[sS]ocial [wW]ork")
sw.news_sample <- sw.news_sample %>% 
    mutate(title_pred = str_detect(title, pattern = "[sS]ocial [wW]ork")) 
            # see below to do case insensitivity


sw.news_sample$title_pred <- case_when(sw.news_sample$title_pred == T ~ 1, 
                                       sw.news_sample$title_pred == F ~ 0)

title_pred_label = c("No phrase", "Has phrase")
sw.news_sample$title_pred <- factor(sw.news_sample$title_pred, labels = title_pred_label)
table(sw.news_sample$title_pred, exclude = NULL)

# text
# ====
    # Num of times phrase appears
str_view_all(sw.news_sample$text[84], 
             regex(pattern = "social-work|social work", ignore_case = T))
    # 76 has social-work

sw.news_sample <- sw.news_sample %>% 
    mutate(phrase_pred = str_count(text, regex(pattern = "social-work|social work", ignore_case = T)))
summary(sw.news_sample$phrase_pred)

require(ggplot2)
ggplot(sw.news_sample, aes(x = phrase_pred) ) + geom_bar()

View_phrase_pred <- sw.news_sample %>% 
    filter(phrase_pred > 18)

# section
# ========
unique(sw.news_sample$section[1:10])

# Geographic
# =========
    # this predictor will have three conditions: In Singapore, not in Singapore, Not sure
unique(sw.news_sample$geographic[1:20]) # there are NA

str_view(sw.news_sample$geographic, regex(pattern = "singapore", ignore_case = T))

sw.news_sample <- sw.news_sample %>% 
    mutate(geographic_pred = str_detect(geographic, regex(pattern = "singapore", ignore_case = T)))

table(sw.news_sample$geographic_pred , exclude = NULL) 
    # missing values because there are missing for geographic
    # i will fill in the NA using two conditions below

# Replace missing values in geographic_pred conditional on the following two conditions:
# In text: the word Singapore appears
# In section: the word Home or Singapore appears (Home => "Home news" Singapre News")
# If still missing, then fill value "not sure"

section_pattern <- "home|singapore"
sw.news_sample <- sw.news_sample %>% 
    mutate(sg_present_text    = str_detect(text,    regex(pattern = "singapore", ignore_case = T)),
           sg_present_section = str_detect(section, regex(pattern = section_pattern, ignore_case = T))
    )
    
view_phrase <-  select(sw.news_sample, text, sg_present_text, section, 
                                           sg_present_section, geographic, geographic_pred )


sw.news_sample <- sw.news_sample %>% 
        mutate(geographic_pred = replace(geographic_pred, 
                                         sg_present_text == T | sg_present_section == T, T))
# the abv command replace geographic_pred with TRUE if it meets the both conditions
# But there is still missing 

table(sw.news_sample$geographic_pred, exclude = NULL) 

sw.news_sample$geographic_pred <- case_when(sw.news_sample$geographic_pred == T ~ "SG",
                                            sw.news_sample$geographic_pred == F ~ "Not SG",
                                           is.na(sw.news_sample$geographic_pred) ~ "unsure")
table(sw.news_sample$geographic_pred, exclude = NULL) 

sw.news_sample$geographic_pred <- factor(sw.news_sample$geographic_pred)
table(sw.news_sample$geographic_pred)                             

table(sw.news_sample$geographic_pred)



# key words in text
# =================
    # keywords_pred = Did key words appeared? (yes/no)
    # key words = "social service", "ministry", "ncss", "vwo", "sasw" "fsc"

pattern_keywords <- "social service|vwo|voluntary welfare organization|family service centre|fsc|ncss|singapore association of social workers|sasw"

#str_view(sw.news_sample$text[80], regex(pattern = pattern_keywords, ignore_case = T))

sw.news_sample  <- sw.news_sample %>% 
        mutate(keywords_pred = str_detect(sw.news_sample$text, 
                                          regex(pattern = pattern_keywords, ignore_case = T)))

sw.news_sample$keywords_pred <- case_when(sw.news_sample$keywords_pred == T ~ 1, 
                                          sw.news_sample$keywords_pred == F ~ 0)

view_keyword <- select(sw.news_sample, text, id , keywords_pred)
table(sw.news_sample$keywords_pred, exclude = NULL)

sw.news_sample$keywords_pred <- factor(sw.news_sample$keywords_pred, labels = c("no", "yes"))
table(sw.news_sample$keywords_pred, exclude = NULL)

# subject
# ========
      # Did list of subject appeared? (yes/no)
head(sw.news_sample$subject, n=100)

pattern_subject = "family services|abuse|family|children|youth|mental health|domestic violence|welfare|poverty|low income"

str_view(sw.news_sample$subject, regex(pattern = pattern_subject, ignore_case = T))
sw.news_sample <- sw.news_sample %>% 
    mutate(subject_pred = str_detect(subject, 
                                     regex(pattern = pattern_subject, ignore_case = T)))

sw.news_sample$subject_pred <- case_when(sw.news_sample$subject_pred == T ~ 1,
                                         sw.news_sample$subject_pred == F ~ 0,
                                         is.na(sw.news_sample$subject_pred)  ~ 0)

sw.news_sample$subject_pred <- factor(sw.news_sample$subject_pred, labels = c("no", "yes"))

table(sw.news_sample$subject_pred , exclude = NULL)


# REDUCE DATASET
# ==============
sw.news_sample_reduced <- sw.news_sample %>% 
    select(1:3, ends_with("pred"))
summary(sw.news_sample_reduced) # check no NA

# Create test and train sets
#############################

# Merge in the handcoded labels "class"
library(readxl)
handcoded <- read_xlsx("cr_data/sw_news_randomsample1000_coded.xlsx", 
                       col_names = T, 
                       )
tail(handcoded, n=5) # check last five rows
tail(sw.news_sample_reduced, n=5) # check last five rows

table(handcoded$classify) # no information rate = 70% 

handcoded <- handcoded %>% select(2:3, classify) # keep 3 vars 

validation <- inner_join(sw.news_sample_reduced, handcoded)
glimpse(validation)
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
predictors1 <- c("phrase_pred", "title_pred", "geographic_pred", "subject_pred", "keywords_pred")
formula1 <- formula(paste("classify ~", 
                    paste(predictors1, collapse=" + "))
                    ) 
formula1


# model 2 interaction1
predictors_interact <- c("phrase_pred", "title_pred", 
                         "geographic_pred", "subject_pred", "keywords_pred", 
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

p <- predict(sw_news_glm, test, type = "response")

# ROC
# ====
library(caTools)
colAUC(p, test$classify, plotROC = T)
# calculates AUC = .85   

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
# calculates AUC = .85 -> not difference from model1   

thresh <- .5

y_or_n2 <- ifelse(p2 > thresh, "y", "n")
confusionMatrix(as.factor(y_or_n2), as.factor(test$classify), positive = "y")



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

model_lm # Accuracy = .78
model_lm$finalModel 
exp(model_lm$finalModel$coefficients) # odds ratio 

model_lm$resample
sd(model_lm$resample$Accuracy)

model_lm$results  # accuracy, kappa, accuracySD, kappaSD
                  # accuracy = average across the ten [model_lm$resample]
                  # ROC = .873, SD = .03
                



# CV repeated GLM
set.seed(222)
model_lm_repeatecv <- train(
                        form = formula1, 
                        data = validation,
                        method = "glm",
                        trControl = myControl_repeatedcv
)

model_lm_repeatecv # Accuracy = .79 
model_lm_repeatecv$finalModel 
exp(model_lm_repeatecv$finalModel$coefficients) # odds ratio 

model_lm_repeatecv$resample
sd(model_lm_repeatecv$resample$Accuracy)

model_lm_repeatecv$results # ROC = .873, SD = .03



# GBM model (Stochastic Gradient Boosting)
# =======================================
set.seed(222)
model_gbm <- train(
    form = formula1, 
    data = validation,
    method = "gbm",
    trControl = myControl
)

model_gbm # Accuracy = .79 do we use this or below command?
model_gbm$finalModel 
#exp(model_gbm$finalModel$coefficients) # odds ratio 

model_gbm$resample 
sd(model_gbm$resample$Accuracy)

model_gbm$results


# RF
# =======================================
set.seed(222)
model_rf <- train(
    form = formula1, 
    data = validation,
    method = "ranger",
    trControl = myControl
)

model_rf # Accuracy = .80
model_rf$finalModel 
#exp(model_gbm$finalModel$coefficients) # odds ratio 

model_rf$resample
sd(model_rf$resample$Accuracy)

model_rf$results


# CHOSEN MODEL
##############
final.train.model <- model_lm_repeatecv # GLM with repeated CV 
getwd()
save(file = "cr_data/final_trained_model", final.train.model)



