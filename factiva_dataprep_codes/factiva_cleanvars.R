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

factiva_clean <- factiva_df
rm(factiva_df)
glimpse(factiva_clean)

## Section A: Clean and Keeping
###############################

# KEEP VARS THAT I NEED
# =====================
factiva_clean[1]
factiva_clean <- factiva_clean %>% select(file_id, text, datetimestamp, 
                                          heading, origin, section, subject,
                                          coverage)
# RENAME VARS
# ==========

# create id 
factiva_clean <- mutate(factiva_clean, id_factiva = row_number()) %>% 
                 relocate(id_factiva, .before = text)
summary(factiva_clean$id_factiva)

# TITLE
factiva_clean <- factiva_clean %>% rename(title = heading)

# GEOGRAPHIC
factiva_clean <- factiva_clean %>% rename(geographic = coverage)

# SOURCE
factiva_clean <- factiva_clean %>% rename(source = origin)

# DATE
factiva_clean <- factiva_clean %>% rename(pub.date = datetimestamp)


## Section B: PREPARE PREDICTORS 
###############################

# title 
# =====
    # does Social work appear in title? (yes/no)

#str_view(factiva_clean$title, pattern = "[sS]ocial [wW]ork")
factiva_clean <- factiva_clean %>% 
    mutate(title_pred = str_detect(title, pattern = "[sS]ocial [wW]ork")) 

factiva_clean$title_pred <- case_when(factiva_clean$title_pred == T ~ 1, 
                                      factiva_clean$title_pred == F ~ 0)

title_pred_label = c("No phrase", "Has phrase")
factiva_clean$title_pred <- factor(factiva_clean$title_pred, labels = title_pred_label)
table(factiva_clean$title_pred, exclude = NULL)



# text
# ====
# Num of times phrase appears
# str_view_all(factiva_clean$text[1],  regex(pattern = "social-work|social work", ignore_case = T))


factiva_clean <- factiva_clean %>% 
    mutate(phrase_pred = str_count(text, regex(pattern = "social-work|social work", ignore_case = T)))
summary(factiva_clean$phrase_pred)

#require(ggplot2)
#ggplot(factiva_clean, aes(x = phrase_pred) ) + geom_bar()

View.phrase_pred <- factiva_clean %>% 
    filter(phrase_pred > 18)

factiva_clean <- factiva_clean %>% 
    mutate(log.phrase_pred = log(phrase_pred + .0001)) %>% 
    relocate(log.phrase_pred, .after = phrase_pred)
summary(factiva_clean$log.phrase_pred)


# section
# ========
unique(factiva_clean$section[1:50])
str_view(factiva_clean$section, regex(pattern = "life|Arts|Entertainment|lifestyle|sport", ignore_case = T))

factiva_clean <- factiva_clean %>% 
    mutate(section_pred = str_detect(section, regex(pattern = "life|Arts|Entertainment|lifestyle|sport", ignore_case = T)))

table(factiva_clean$section_pred, exclude= F )

factiva_clean <- factiva_clean %>% mutate(section_pred = case_when(section_pred == TRUE ~"life/Arts/Entertainment/Sports",
                                                                   TRUE ~ "Not"))

# Geographic
# =========
# this predictor will have three conditions: In Singapore, not in Singapore, Not sure
unique(factiva_clean$geographic[1:20]) # there are NA

#str_view(factiva_clean$geographic, regex(pattern = "singapore", ignore_case = T))

factiva_clean <- factiva_clean %>% 
    mutate(geographic_pred = str_detect(geographic, regex(pattern = "singapore", ignore_case = T)))

table(factiva_clean$geographic_pred , exclude = NULL) 
    # missing values because there are missing for geographic
    # i will fill in the NA using two conditions below

# Replace missing values in geographic_pred conditional on the following two conditions:
# In text: the word Singapore appears
# In section: the word Home or Singapore appears (Home => "Home news" Singapre News")
# If still missing, then fill value "not sure"

section_pattern <- "home|singapore"
section_pattern_negate <- "asia|asean|world|malaysia|Across the Causeway|Foreign News"  
    # news in the asia/world section will be false i.e. not Singapore

#str_view_all(factiva_clean$section, regex(pattern = section_pattern_negate, ignore_case = T))

factiva_clean <- factiva_clean %>% 
    mutate(
        sg_present_text    = str_detect(text, regex(pattern = "singapore", ignore_case = T)),
        sg_present_section = str_detect(section, regex(pattern = section_pattern, ignore_case = T)),
        asia_world_section = str_detect(section, regex(pattern = section_pattern_negate, ignore_case = T))
    )

view.phrase <-  select(factiva_clean, text, sg_present_text, section, 
                       sg_present_section, asia_world_section, geographic, geographic_pred )


factiva_clean <- factiva_clean %>% 
    mutate(geographic_pred = replace(geographic_pred, 
                                     sg_present_text == T | sg_present_section == T, T),
           geographic_pred = replace(geographic_pred, 
                                     asia_world_section == T , F)
    )

# the abv command replace geographic_pred with TRUE if it meets the both conditions
# But there is still missing 

table(factiva_clean$geographic_pred, exclude = NULL) 

factiva_clean$geographic_pred <- case_when(factiva_clean$geographic_pred == T ~ "SG",
                                           factiva_clean$geographic_pred == F ~ "Not SG",
                                           is.na(factiva_clean$geographic_pred) ~ "unsure")

table(factiva_clean$geographic_pred, exclude = NULL) 

factiva_clean$geographic_pred <- factor(factiva_clean$geographic_pred)
table(factiva_clean$geographic_pred)                             


# key words in text
# =================
    # keywords_pred = Did key words appeared? (yes/no)
    # key words = "social service", "social-service", "ministry", "ncss", "vwo", "sasw" "fsc"

pattern_keywords <- "social service|social-service|vwo|voluntary welfare organization|family service centre|fsc|ncss|singapore association of social workers|sasw|s r nathan|ann wee"


factiva_clean  <- factiva_clean %>% 
    mutate(keywords_pred = str_detect(text, 
                                      regex(pattern = pattern_keywords, ignore_case = T)))

factiva_clean$keywords_pred <- case_when(factiva_clean$keywords_pred == T ~ 1, 
                                         factiva_clean$keywords_pred == F ~ 0)

view.keyword <- select(factiva_clean, text, id_factiva , keywords_pred)
table(factiva_clean$keywords_pred, exclude = NULL)

factiva_clean$keywords_pred <- factor(factiva_clean$keywords_pred, labels = c("no", "yes"))
table(factiva_clean$keywords_pred, exclude = NULL)


# subject
# ========
# Did list of subject appeared? (yes/no)
head(factiva_clean$subject, n=100)

pattern_subject = "family services|abuse|family|children|youth|mental health|domestic violence|welfare|poverty|low income|elderly|senior citizen|criminal|crime|corrections|
nursing home|counsel|marriage|neglect|university"

#str_view(factiva_clean$subject, regex(pattern = pattern_subject, ignore_case = T))
factiva_clean <- factiva_clean %>% 
    mutate(subject_pred = str_detect(subject, 
                                     regex(pattern = pattern_subject, ignore_case = T)))

factiva_clean$subject_pred <- case_when(factiva_clean$subject_pred == T ~ 1,
                                        factiva_clean$subject_pred == F ~ 0,
                                     is.na(factiva_clean$subject_pred)  ~ 0)

factiva_clean$subject_pred <- factor(factiva_clean$subject_pred, labels = c("no", "yes"))

table(factiva_clean$subject_pred , exclude = NULL)


# Source
########

unique(factiva_clean$source)

# Today
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Today (Singapore) - Online" , "Today")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source ==("TODAY (Singapore)") , "Today")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source ==("TODAY") , "Today")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source ==("Today") , "Today")

# ST
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="The Straits Times (Singapore)" , "Straits Times")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="The Straits Times" , "Straits Times")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Straits Times" , "Straits Times")

# BT
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="The Business Times Singapore" , "Business Times")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Business Times (Singapore)" , "Business Times")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Business Times Singapore" , "Business Times")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Business Times" , "Business Times")

# CNA
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Channel NewsAsia" , "Channel NewsAsia")
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="Channelnewsasia" , "Channel NewsAsia")

# The New Paper
factiva_clean$source_pred <-  replace(factiva_clean$source_pred, factiva_clean$source =="The New Paper" , "The New Paper")


unique(factiva_clean$source)
unique(factiva_clean$source_pred)

factiva_clean$source_pred <- factor(factiva_clean$source_pred)

table(factiva_clean$source_pred, exclude = F)
table(factiva_clean$source, exclude = F)
require(janitor)
tabyl(factiva_clean$source_pred) %>%  adorn_pct_formatting()

NROW(factiva_clean) # N = 10427 

# SECTION C: DUPLICATES AND YEAR 2021
#####################################

# Filer out duplicates and year = 2021 within this dataset
# =========================================================

# Check for year 2021
str_view(factiva_clean$pub.date[1:20], pattern = "\\d{4}")
str(factiva_clean$pub.date)
unique(factiva_clean$pub.date)
factiva_clean$pub.year <- format(factiva_clean$pub.date, "%Y")
unique(factiva_clean$pub.year)
factiva_clean$pub.year <- as.numeric(factiva_clean$pub.year)
table(factiva_clean$pub.year)

# Check for duplicates in titles
factiva_clean$title_duplicates <- duplicated(factiva_clean$title)

View.duplicates <- factiva_clean %>% 
    select(id_factiva, title, text, pub.date, title_duplicates) %>% 
    filter(title_duplicates == T)
# View(View.duplicates) # these obs are duplicates but will not include the original ones

# Remove both year 2021 and duplicates in titles
factiva_clean <- factiva_clean %>% 
    filter(pub.year != "2021") %>%  # remove year 2021
    filter(title_duplicates != T)  %>%  # remove duplicates in titles
    select(-(pub.year), -(title_duplicates)) # remove these two variables

sum(duplicated(factiva_clean$title)) # check again if have duplicates

nrow(factiva_clean) # N = 9869 from 10427 (after removing year 2021 and duplicates in titles)
10427 - 9869


# Save dataset
# ===========
sw.news <- factiva_clean
save(sw.news, file = "cr_data/factiva/sw.news.RData")


