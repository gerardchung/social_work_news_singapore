########################################
# TEXT-MING SOCIAL WORK NEWS 
########################################
# This do file is to clean vars from factiva extracted data that
# The vars will be cleaned according to the vars cleaned in Nexis dataset: news_sw_training.R

## Section A 
# I will first clean the vars by renaming the vars and keeping the relevant  vars 
# according to Nexis dataset in news_sw_training.R

## Section B 
# I then prepare predictors according to news_sw_training.R

## Section C
# Remove  year 2021
# Check for duplicates and remove



rm(list = ls())

#library(textreadr)
library(stringr)
library(dplyr)

getwd()

# load RData dataset
####################


load(file = "source_data_factiva/factiva_df.RData")

favtiva_clean <- factiva_df
rm(factiva_df)
glimpse(favtiva_clean)

## Section A: Clean and Keeping
###############################

# KEEP VARS THAT I NEED
# =====================
favtiva_clean[1]
favtiva_clean <- favtiva_clean %>% select(file_id, text, datetimestamp, 
                                          heading, origin, section, subject,
                                          coverage)
# RENAME VARS
# ==========

# create id 
favtiva_clean <- mutate(favtiva_clean, id_factiva = row_number()) %>% 
                 relocate(id_factiva, .before = text)
summary(favtiva_clean$id_factiva)

# TITLE
favtiva_clean <- favtiva_clean %>% rename(title = heading)

# GEOGRAPHIC
favtiva_clean <- favtiva_clean %>% rename(geographic = coverage)

# SOURCE
favtiva_clean <- favtiva_clean %>% rename(source = origin)

# DATE
favtiva_clean <- favtiva_clean %>% rename(pub.date = datetimestamp)

# PREPARE PREDICTORS 
####################

# title 
# =====
    # does Social work appear in title? (yes/no)

#str_view(favtiva_clean$title, pattern = "[sS]ocial [wW]ork")
favtiva_clean <- favtiva_clean %>% 
    mutate(title_pred = str_detect(title, pattern = "[sS]ocial [wW]ork")) 

favtiva_clean$title_pred <- case_when(favtiva_clean$title_pred == T ~ 1, 
                                      favtiva_clean$title_pred == F ~ 0)

title_pred_label = c("No phrase", "Has phrase")
favtiva_clean$title_pred <- factor(favtiva_clean$title_pred, labels = title_pred_label)
table(favtiva_clean$title_pred, exclude = NULL)


# text
# ====
# Num of times phrase appears
# str_view_all(favtiva_clean$text[1],  regex(pattern = "social-work|social work", ignore_case = T))


favtiva_clean <- favtiva_clean %>% 
    mutate(phrase_pred = str_count(text, regex(pattern = "social-work|social work", ignore_case = T)))
summary(favtiva_clean$phrase_pred)

#require(ggplot2)
#ggplot(favtiva_clean, aes(x = phrase_pred) ) + geom_bar()

View.phrase_pred <- favtiva_clean %>% 
    filter(phrase_pred > 18)

favtiva_clean <- favtiva_clean %>% 
    mutate(log.phrase_pred = log(phrase_pred + .0001)) %>% 
    relocate(log.phrase_pred, .after = phrase_pred)
summary(favtiva_clean$log.phrase_pred)


# section
# ========
unique(favtiva_clean$section[1:10])

# Geographic
# =========
# this predictor will have three conditions: In Singapore, not in Singapore, Not sure
unique(favtiva_clean$geographic[1:20]) # there are NA

#str_view(favtiva_clean$geographic, regex(pattern = "singapore", ignore_case = T))

favtiva_clean <- favtiva_clean %>% 
    mutate(geographic_pred = str_detect(geographic, regex(pattern = "singapore", ignore_case = T)))

table(favtiva_clean$geographic_pred , exclude = NULL) 
    # missing values because there are missing for geographic
    # i will fill in the NA using two conditions below

# Replace missing values in geographic_pred conditional on the following two conditions:
# In text: the word Singapore appears
# In section: the word Home or Singapore appears (Home => "Home news" Singapre News")
# If still missing, then fill value "not sure"

section_pattern <- "home|singapore"
section_pattern_negate <- "asia|world|malaysia|Across the Causeway|Foreign News"  
    # news in the asia/world section will be exclude i.e. False

#str_view_all(favtiva_clean$section, regex(pattern = section_pattern_negate, ignore_case = T))

favtiva_clean <- favtiva_clean %>% 
    mutate(
        sg_present_text    = str_detect(text, regex(pattern = "singapore", ignore_case = T)),
        sg_present_section = str_detect(section, regex(pattern = section_pattern, ignore_case = T)),
        asia_world_section = str_detect(section, regex(pattern = section_pattern_negate, ignore_case = T))
    )

view.phrase <-  select(favtiva_clean, text, sg_present_text, section, 
                       sg_present_section, asia_world_section, geographic, geographic_pred )


favtiva_clean <- favtiva_clean %>% 
    mutate(geographic_pred = replace(geographic_pred, 
                                     sg_present_text == T | sg_present_section == T, T),
           geographic_pred = replace(geographic_pred, 
                                     asia_world_section == T , F)
    )

# the abv command replace geographic_pred with TRUE if it meets the both conditions
# But there is still missing 

table(favtiva_clean$geographic_pred, exclude = NULL) 

favtiva_clean$geographic_pred <- case_when(favtiva_clean$geographic_pred == T ~ "SG",
                                           favtiva_clean$geographic_pred == F ~ "Not SG",
                                           is.na(favtiva_clean$geographic_pred) ~ "unsure")

table(favtiva_clean$geographic_pred, exclude = NULL) 

favtiva_clean$geographic_pred <- factor(favtiva_clean$geographic_pred)
table(favtiva_clean$geographic_pred)                             


# key words in text
# =================
    # keywords_pred = Did key words appeared? (yes/no)
    # key words = "social service", "social-service", "ministry", "ncss", "vwo", "sasw" "fsc"

pattern_keywords <- "social service|social-service|vwo|voluntary welfare organization|family service centre|fsc|ncss|singapore association of social workers|sasw|s r nathan|ann wee"


favtiva_clean  <- favtiva_clean %>% 
    mutate(keywords_pred = str_detect(text, 
                                      regex(pattern = pattern_keywords, ignore_case = T)))

favtiva_clean$keywords_pred <- case_when(favtiva_clean$keywords_pred == T ~ 1, 
                                         favtiva_clean$keywords_pred == F ~ 0)

view.keyword <- select(favtiva_clean, text, id_factiva , keywords_pred)
table(favtiva_clean$keywords_pred, exclude = NULL)

favtiva_clean$keywords_pred <- factor(favtiva_clean$keywords_pred, labels = c("no", "yes"))
table(favtiva_clean$keywords_pred, exclude = NULL)


# subject
# ========
# Did list of subject appeared? (yes/no)
head(favtiva_clean$subject, n=100)

pattern_subject = "family services|abuse|family|children|youth|mental health|domestic violence|welfare|poverty|low income|elderly|senior citizen|criminal|crime|corrections|
nursing home|counsel|marriage|neglect|university"

#str_view(favtiva_clean$subject, regex(pattern = pattern_subject, ignore_case = T))
favtiva_clean <- favtiva_clean %>% 
    mutate(subject_pred = str_detect(subject, 
                                     regex(pattern = pattern_subject, ignore_case = T)))

favtiva_clean$subject_pred <- case_when(favtiva_clean$subject_pred == T ~ 1,
                                        favtiva_clean$subject_pred == F ~ 0,
                                     is.na(favtiva_clean$subject_pred)  ~ 0)

favtiva_clean$subject_pred <- factor(favtiva_clean$subject_pred, labels = c("no", "yes"))

table(favtiva_clean$subject_pred , exclude = NULL)


# Source
########

unique(favtiva_clean$source)

# Today
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Today (Singapore) - Online" , "Today")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source ==("TODAY (Singapore)") , "Today")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source ==("TODAY") , "Today")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source ==("Today") , "Today")

# ST
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="The Straits Times (Singapore)" , "Straits Times")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="The Straits Times" , "Straits Times")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Straits Times" , "Straits Times")

# BT
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="The Business Times Singapore" , "Business Times")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Business Times (Singapore)" , "Business Times")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Business Times Singapore" , "Business Times")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Business Times" , "Business Times")

# CNA
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Channel NewsAsia" , "Channel NewsAsia")
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="Channelnewsasia" , "Channel NewsAsia")

# The New Paper
favtiva_clean$source_pred <-  replace(favtiva_clean$source_pred, favtiva_clean$source =="The New Paper" , "The New Paper")


unique(favtiva_clean$source)
unique(favtiva_clean$source_pred)

favtiva_clean$source_pred <- factor(favtiva_clean$source_pred)

table(favtiva_clean$source_pred, exclude = F)
table(favtiva_clean$source, exclude = F)
require(janitor)
tabyl(favtiva_clean$source_pred) %>%  adorn_pct_formatting()


