# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# ANALYSIS OF TITLES
#####################
    # RQ: 
    ## Sentiment of titles?
    ## Words used in titles?
    ## Adjectives used to describe social work?
    ## Topics 

# Note: I think the focus on titles will have limited insights. It is more useful
# to look in the content as well. This will be done later. Doing titles analysis
# can help guide the content analysis later

# Things to do:
    # Look at words, phrases, adjectives, and verbs used to describe SW in titles
    # Need to remove titles with no key phrase "social work" 
    # Need to remove titles with only one word (you cannot do POS on one word sentences)
    # Convert to corpus
    # Do POS with spacyr
    # Look at freq of adjectives; word clouds 


    ## Section A: Convert to corpus
    ## Section B: Look at sentiment analysis of titles
    ## Section C: Spacyr and look at key words and adjectives used
    ## Section D: {Reduce to titles with at least three and has key phrase in title}
    ## Section B: Convert to corpus

# LOAD DATASET
##############

#renv::init() 
rm(list = ls())

library(tidyverse)
library(quanteda)
library(janitor)
library(ggplot2)
library(stringr)

getwd()
load(file = "an_data/factiva/an_swnews_cleaned.RData") 
title_df <- 
    an_swnews_cleaned %>% 
    select(-text)

# Titles with one word
######################
    # Checks titles to see how many words 
    # Will drop docs with only one word

title_df$one_wrd <-  stringi::stri_count(title_df$title, regex="\\w+")
    # count number of words in each title

title_df %>% tabyl(one_wrd)
one_wrd.df <- title_df[title_df$one_wrd ==1] # titles  with one word
glimpse(one_wrd.df$title) # These one-worded titles will not be useful - drop in the next command

title_df <- title_df[title_df$one_wrd !=1] # remove docs with only 1 word in title
NROW(title_df) # 7826 (after dropping 22 docs)

# Titles with phrase
#####################

str_view_all(title_df$title[300:350], regex(pattern = "social work|social worker|social-work"))

title_df$has.phrase <- str_detect(title_df$title, 
                         regex(pattern = "social work|social worker|social-work" , ignore_case = T ))

#View(title_df %>% select(title, has.phrase))
title_df %>%  tabyl(has.phrase) # 418 doc has the phrase

title_phraseDF <- title_df[has.phrase == T]
glimpse(title_phraseDF)
# Convert corpus
###############

title_corp <- corpus(title_phraseDF,
                     text_field = "title")
summary(summary(title_corp))


# Sentiment analysis
#####################
    # using tokens will make them into lists

# Convert into tokens and look-up using LSD dictionary
# ==================================================
title_tokens <- tokens(title_corp) # list?
title_tokens_lsd <- tokens_lookup(title_tokens, 
                                  dictionary = data_dictionary_LSD2015,
                                  exclusive = T,
                                  nested_scope = "dictionary")
    # exclusive = F will not delete words that are not in LSD
    # nested_scope is relevant for LSD to avoid double-counting of negated and non-negated terms
            
title_tokens_lsd[1:10]
    # negative here does not mean something bad about social work.
    # negative is very broad which can mean that the social work profession has been mistaken e.g. a volunteer
title_phraseDF$title[5]

# Convert back into DFM and bind in meta-doc vars
# ==================================================
lsd_dfm <- dfm(title_tokens_lsd)
lsd_df <- 
    convert(lsd_dfm, to = "data.frame") %>% 
    cbind(docvars(lsd_dfm)) 
     # https://stackoverflow.com/questions/60419692/how-to-convert-dfm-into-dataframe-but-keeping-docvars
     # convert(lsd_dfm, to = "data.frame") by iself will not keep the meta-doc vars
     # cbind(docvars(lsd_dfm)) will do it 

glimpse(lsd_df)

# Calculate sentiment value
# ==================================================

# total number of terms per document. 
# ============================================================
all_words = dfm(title_tokens)
lsd_df$total_words <- rowSums()
lsd_df$total_words = rowSums(all_words)

# Calculate valence
# ================
tabyl(lsd_df$neg_positive)
tabyl(lsd_df$neg_negative)
tabyl(lsd_df$positive)

lsd_df$positiveSum <- lsd_df$positive + lsd_df$neg_negative
lsd_df$negativeSum <- lsd_df$negative + lsd_df$neg_positive
tabyl(lsd_df$positiveSum)
tabyl(lsd_df$negativeSum)

lsd_df$valence <- 
    (lsd_df$positiveSum/lsd_df$total_words) -
    (lsd_df$negativeSum/lsd_df$total_words)

lsd_year <- 
    lsd_df %>% 
    group_by(pub_year) %>% 
    summarise(valence_mean = mean(valence)) 
View(lsd_year %>% select(pub_year, valence_mean))
                
# Valence by year
###################
    # https://ggplot2.tidyverse.org/reference/geom_smooth.html
p <- ggplot(lsd_year, mapping = aes(x = pub_year, valence_mean))
p + geom_point() +
   # geom_line() +
    geom_smooth(method = "lm")

library(moderndive)
valence_lm <- lm(valence_mean ~ pub_year, data = lsd_year)
get_regression_table(valence_lm)



###
title_dfm_lsd <- dfm(title_tokens_lsd)
head(title_dfm_lsd[,1])
title_phraseDF$negative <- as.numeric(title_dfm_lsd[,1])
title_phraseDF$positive <- as.numeric(title_dfm_lsd[,2])
title_phraseDF$neg_postive <- as.numeric(title_dfm_lsd[,3])
title_phraseDF$neg_negative <- as.numeric(title_dfm_lsd[,4])

title_phraseDF %>% tabyl(negative)
title_phraseDF %>% tabyl(positive)
title_phraseDF %>% tabyl(neg_postive)
title_phraseDF %>% tabyl(neg_negative)

title_dfm_lsd %>% tabyl(title_dfm_lsd[,1])
title_dfm_lsd@Dimnames$features["negative"]
