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

tail(sw.news$id, n=5)

# CLEAN VARIABLES & CREATE PREDICTORS
######################################
######################################
    # all the variables named with "_pred" suffix are predictors

# title 
# =====

#str_view(sw.news$title[1:10], pattern = "[sS]ocial [wW]ork")
sw.news <- sw.news %>% 
    mutate(title_pred = str_detect(title, pattern = "[sS]ocial [wW]ork")) 
# see below to do case insensitivity


sw.news$title_pred <- case_when(sw.news$title_pred == T ~ 1, 
                                sw.news$title_pred == F ~ 0)

title_pred_label = c("No phrase", "Has phrase")
sw.news$title_pred <- factor(sw.news$title_pred, labels = title_pred_label)
table(sw.news$title_pred)

# text
# ====
# Num of times phrase appears

#str_view_all(sw.news$text[84], regex(pattern = "social-work|social work", ignore_case = T))
        # 76 has social-work

sw.news <- sw.news %>% 
    mutate(phrase_pred = str_count(text, regex(pattern = "social-work|social work", ignore_case = T)))
summary(sw.news$phrase_pred)

require(ggplot2)
ggplot(sw.news, aes(x = phrase_pred) ) + geom_bar()

View_phrase_pred <- sw.news %>% 
    filter(phrase_pred > 18)

# section
# ========
unique(sw.news$section[1:10])


# Geographic
# =========
    # this predictor will have three conditions
    # In Singapore, not in Singapore, Not sure
unique(sw.news$geographic[1:20]) # there are NA

sw.news <- sw.news %>% 
    mutate(geographic_pred = str_detect(geographic, regex(pattern = "singapore", ignore_case = T)))

table(sw.news$geographic_pred, exclude = NULL) 
    # missing values because there are missing for geographic
    # i will fill in the NA using two conditions below

# Replace missing values in geographic_pred conditional on the following two conditions:
    # In text: the word Singapore appears
    # In section: the word Home or Singapore appears (Home => "Home news" Singapre News")
    # If still missing, then fill value "not sure"
section_pattern <- "home|singapore"
#str_view(sw.news$section[30:50], regex(pattern = section_pattern, ignore_case = T ))

sw.news <- sw.news %>% 
    mutate(sg_present_text    = str_detect(text,    regex(pattern = "singapore", ignore_case = T)),
           sg_present_section = str_detect(section, regex(pattern = section_pattern, ignore_case = T))
    )

table(sw.news$geographic_pred, exclude = NULL)

sw.news <- sw.news %>% 
    mutate(geographic_pred = replace(geographic_pred, 
                                     sg_present_text == T | sg_present_section == T, T)) 
    # the abv command replace geographic_pred with TRUE if it meets the both conditions
    # But there is still missing 
    
table(sw.news$geographic_pred, exclude = NULL) 

# view_geo <-  select(sw.news, text, sg_present_text, section, 
#                    sg_present_section, geographic, geographic_pred )

sw.news$geographic_pred <- case_when(sw.news$geographic_pred == T ~ "SG",
                                     sw.news$geographic_pred == F ~ "Not SG",
                                     is.na(sw.news$geographic_pred) ~ "unsure")

table(sw.news$geographic_pred, exclude = NULL) 

#sw.news_sample$geographic_pred <- recode(sw.news_sample$geographic_pred, '1' = 2, '0' =1, .missing = #0 )

sw.news$geographic_pred <- factor(sw.news$geographic_pred)
table(sw.news$geographic_pred)                             


# key words in text
# =================
    # keywords_pred = Did key words appeared? (yes/no)
    # key words = "social service", "ministry", "ncss", "vwo", "sasw" "fsc"

pattern_keywords <- "social service|vwo|voluntary welfare organization|family service centre|fsc|ncss|singapore association of social workers|sasw"

#str_view_all(sw.news$text[80], regex(pattern = pattern_keywords, ignore_case = T))

sw.news  <- sw.news %>% 
    mutate(keywords_pred = str_detect(text, 
                                      regex(pattern = pattern_keywords, ignore_case = T)))

sw.news$keywords_pred <- case_when(sw.news$keywords_pred == T ~ 1, 
                                   sw.news$keywords_pred == F ~ 0)

view_keyword <- select(sw.news, text, id , keywords_pred)
table(sw.news$keywords_pred, exclude = NULL)

sw.news$keywords_pred <- factor(sw.news$keywords_pred, labels = c("no", "yes"))
table(sw.news$keywords_pred, exclude = NULL)


# subject
# ========
    # Did list of subject appeared? (yes/no)
head(sw.news$subject, n=100)

pattern_subject = "family services|abuse|family|children|youth|mental health|domestic violence|welfare|poverty|low income"

#str_view_all(sw.news$subject, regex(pattern = pattern_subject, ignore_case = T))

sw.news <- sw.news %>% 
    mutate(subject_pred = str_detect(subject, 
                                     regex(pattern = pattern_subject, ignore_case = T)))

sw.news$subject_pred <- case_when(sw.news$subject_pred == T ~ 1,
                                  sw.news$subject_pred == F ~ 0,
                               is.na(sw.news$subject_pred)  ~ 0)

sw.news$subject_pred <- factor(sw.news$subject_pred, labels = c("no", "yes"))

table(sw.news$subject_pred , exclude = NULL)

# PREDICT TRAINED MODEL ON REMAINING DATASET
# =============================================

# Join in handcoded dataset with 1000 observation
# ===============================================

# Merge in the handcoded labels "classify"
library(readxl)
handcoded <- read_xlsx("cr_data/sw_news_randomsample1000_coded.xlsx", 
                       col_names = T, 
)
tail(handcoded, n=5) # check last five rows
table(handcoded$classify) 

handcoded <- handcoded %>% select(2, title, classify) # keep 3 vars 

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

# Save into excel for checking on predicted docs
xlsx::write.xlsx(sw.news.predict, "cr_data/sw_news_checkingprediction344.xlsx")


# MERGE BACK IN WITH THE MAIN DATAFRAME 
#########################################

glimpse(sw.news.predict)
sw.news.predict <- select(sw.news.predict, id, title, text, geographic_pred, classify)

table(sw.news.labelled$classify, exclude = F)
sw.news.labelled_full <- left_join(sw.news.labelled, sw.news.predict, by = c("id", "title"))
table(sw.news.labelled_full$classify.x) # 300+700
table(sw.news.labelled_full$classify.y) # 91 + 253

sw.news.labelled_full <- sw.news.labelled_full %>% 
    mutate(classify = case_when(classify.x == "y" ~ "y", 
                                classify.x == "n" ~ "n",
                                classify.y == "y" ~ "y",
                                classify.y == "n" ~ "n")) 
table(sw.news.labelled_full$classify)
table(sw.news.labelled_full$classify.x)
table(sw.news.labelled_full$classify.y)
391+953
sw.news.labelled_full <- sw.news.labelled_full %>% 
    select(-classify.x, -classify.y)


# SAVE DATASET FOR ANALYSIS
############################
############################

save(sw.news.labelled_full, file = "an_data/an_sw_news.RData") 





