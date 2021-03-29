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

title_df %>% tabyl(one_wrd)
one_wrd.df <- title_df[title_df$one_wrd ==1] # titles  with one word
glimpse(one_wrd.df$title) # These one-worded titles will not be useful - drop in the next command

title_df <- title_df[title_df$one_wrd !=1] # remove docs with only 1 word in title
NROW(title_df) # 7826 (after dropping 22 docs)



