### TEXT-MING SOCIAL WORK NEWS 

# ANALYSIS OF TITLES
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

    ### SECTION A: SENTIMENT ANALYSIS ON ENTIRE TITLES 

# START ######

## LOAD DATASET ##############

#renv::init() 
rm(list = ls())

library(tidyverse)
library(quanteda)
library(janitor)
library(ggplot2)
library(stringr)

# Color theme for all the graphs
blue = "#6892C1"
lightblue = "#add8e6" # "#6892C1"
orange = "#ED713F"

getwd()
load(file = "an_data/factiva/an_swnews_cleaned.RData") 
title_df <- 
    an_swnews_cleaned %>% 
    select(-text)

## TITLE WITH ONE WORD ######################
    # Checks titles to see how many words 
    # Will drop docs with only one word

title_df$one_wrd <-  stringi::stri_count(title_df$title, regex="\\w+")
    # count number of words in each title

title_df %>% tabyl(one_wrd)
one_wrd.df <- title_df[title_df$one_wrd ==1] # titles  with one word
glimpse(one_wrd.df$title) # These one-worded titles will not be useful - drop in the next command

title_df <- title_df[title_df$one_wrd !=1] # remove docs with only 1 word in title
NROW(title_df) # 7826 (after dropping 22 docs)

## TITLE WITH PHRASE #####################

str_view_all(title_df$title[320:400], regex(pattern = "social work|social worker|social-work", ignore_case = T ))

title_df$has.phrase <- str_detect(title_df$title, 
                         regex(pattern = "social work|social worker|social-work" , ignore_case = T ))

#View(title_df %>% select(title, has.phrase))
title_df %>%  tabyl(has.phrase) # 418 doc has the phrase

title_phraseDF <- title_df[has.phrase == T]
glimpse(title_phraseDF)


# SECTION A: SENTIMENT ANALYSIS ON ENTIRE TITLES ##################

## CONVERT CORPUS FOR TITLE SENTIMENT ANALYSIS #####################

remove_phraseDF <- title_phraseDF
str_view(remove_phraseDF$title[320:400], regex(pattern = "(social work\\w*)" , ignore_case = T))
    # detect all social work, social worker, social workers phrases to remove before sentiment analysis

remove_phraseDF$title <- str_remove_all(remove_phraseDF$title, regex(pattern = "(social work\\w*)" , ignore_case = T))

title_corp <- corpus(remove_phraseDF,
                     text_field = "title")
summary(summary(title_corp))
ndoc(title_corp)

## SENTIMENT ANALYSIS #####################

    # using tokens will make them into lists

### Convert into tokens and look-up using LSD dictionary ===== 
title_tokens <- tokens(title_corp, 
                       remove_punct = T) # list?
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

### Convert back into DFM and bind in meta-doc vars ====

lsd_dfm <- dfm(title_tokens_lsd)
lsd_df <- 
    convert(lsd_dfm, to = "data.frame") %>% 
    cbind(docvars(lsd_dfm)) 
     # https://stackoverflow.com/questions/60419692/how-to-convert-dfm-into-dataframe-but-keeping-docvars
     # convert(lsd_dfm, to = "data.frame") by iself will not keep the meta-doc vars
     # cbind(docvars(lsd_dfm)) will do it 

glimpse(lsd_df)

## CALCULATE VALENCE ##########################

### total number of terms per document ======

all_words = dfm(title_tokens)
lsd_df$total_words = rowSums(all_words)

### Calculate valence === 
tabyl(lsd_df$neg_positive)
tabyl(lsd_df$neg_negative)
tabyl(lsd_df$positive)
tabyl(lsd_df$negative)

lsd_df$positiveSum <- lsd_df$positive + lsd_df$neg_negative
lsd_df$negativeSum <- lsd_df$negative + lsd_df$neg_positive
tabyl(lsd_df$positiveSum)
tabyl(lsd_df$negativeSum)

lsd_df$valence <- 
    (lsd_df$positiveSum/lsd_df$total_words) -
    (lsd_df$negativeSum/lsd_df$total_words)

### VALENCE BY YEAR ====
lsd_year <- 
    lsd_df %>% 
    group_by(pub_year) %>% 
    summarise(valence_mean = mean(valence)) 
#View(lsd_year %>% select(pub_year, valence_mean))

    # https://ggplot2.tidyverse.org/reference/geom_smooth.html
p <- ggplot(lsd_year, mapping = aes(x = pub_year, valence_mean))
p + geom_point() +
   # geom_line() +
    geom_smooth(method = "lm")

library(moderndive)
valence_lm <- lm(valence_mean ~ pub_year, data = lsd_year)
get_regression_table(valence_lm)

### VALENCE BY MONTH =====
glimpse(lsd_df)
class(lsd_df$pub_date)

lsd_df <- 
    lsd_df %>% 
    mutate(neg_pos_val = ifelse(valence < 0, 0, 1 ))

lsd_plot_day <- 
    lsd_df %>% 
   filter(valence != 0) %>% 
    filter(source_pred == "Straits Times") %>% 
    ggplot(aes(x = pub_date, y = valence, group =neg_pos_val ))

lsd_plot_day_novalence <- 
    lsd_df %>% 
    filter(valence == 0) %>% 
    filter(source_pred == "Straits Times")

lsd_plot_day + 
    geom_point(alpha = .5 ) +
    geom_smooth(method = "loess") +
#    geom_point(data = lsd_plot_day_novalence, aes(x = pub_date, y = valence)) +
    theme_minimal(base_family = "Roboto Condensed")

lsd_plot_day + 
    geom_point(alpha = .5, aes(color = neg_pos_val)) +
    geom_smooth(method = "lm") +
    geom_point(data = lsd_plot_day_novalence, aes(x = pub_date, y = valence)) +
    theme_minimal(base_family = "Roboto Condensed")

# SECTION B: ANALYSIS ON WORDS BEFORE AND AFTER PHRSE ######

## KWIC social work phrase in title ###################

NROW(title_phraseDF)
glimpse(title_phraseDF$title)

### Convert to corpus ====
    # have to go back and use the initial dataframe because in previous analysis
    # I already removed key phrase social work 
wordcontext_corp <- corpus(title_phraseDF,
                           text_field = "title"
                           )
### Tokenize ====
wordcontext_tok <- tokens(wordcontext_corp, 
                      remove_punct = T)

### Get window and then remove the key phrase ====
keyphrase <- c("social-work", "social work*")

wordcontext_phrase_tok <- tokens_keep(wordcontext_tok, 
                pattern = phrase(keyphrase), window = 5 )
    # tokens_keep keeps the window of +- 5 words including the key phrase
wordcontext_phrase_tok[4]

wordcontext_phrase_tok_removed <- tokens_remove(wordcontext_phrase_tok,
                                        pattern = phrase(keyphrase)) 
    # tokens_remove will remove the key phrase tokens
wordcontext_phrase_tok_removed[4]

## LSD dictionary look-up #######
wordcontext_lsd <- tokens_lookup(wordcontext_phrase_tok_removed,
                                 dictionary = data_dictionary_LSD2015,
                                 exclusive = T,
                                 nested_scope = "dictionary")

### Convert back to DFM -> then DF -> and bind in the meta-doc vars ====

class(wordcontext_lsd)

lsd_wordcontext_dfm <- dfm(wordcontext_lsd) 

lsd_wordcontext_df <- 
    convert(lsd_wordcontext_dfm, to = "data.frame") %>%
    cbind(docvars(lsd_wordcontext_dfm))
    
glimpse(lsd_wordcontext_df)

### Calculate sentiment value [word context] =======

## total number of terms per document ########
all_words_wordcontext = dfm(wordcontext_tok)
lsd_wordcontext_df$total_words = rowSums(all_words_wordcontext)

tabyl(lsd_wordcontext_df$total_words)

### Calculate valence =====

tabyl(lsd_wordcontext_df$neg_positive)
tabyl(lsd_wordcontext_df$neg_negative)
tabyl(lsd_wordcontext_df$positive)
tabyl(lsd_wordcontext_df$negative)

lsd_wordcontext_df$positiveSum <- lsd_wordcontext_df$positive + lsd_wordcontext_df$neg_negative
lsd_wordcontext_df$negativeSum <- lsd_wordcontext_df$negative + lsd_wordcontext_df$neg_positive
tabyl(lsd_wordcontext_df$positiveSum)
tabyl(lsd_wordcontext_df$negativeSum)

lsd_wordcontext_df$valence <- 
    (lsd_wordcontext_df$positiveSum/lsd_wordcontext_df$total_words) -
    (lsd_wordcontext_df$negativeSum/lsd_wordcontext_df$total_words)


## VALENCE BY YEAR [WORD CONTEXT]  ########

lsd_wordcontext_year <- 
    lsd_wordcontext_df %>% 
    group_by(pub_year) %>% 
    summarise(valence_mean = mean(valence)) 
# View(lsd_wordcontext_year %>% select(pub_year, valence_mean))

q <- ggplot(lsd_wordcontext_year, mapping = aes(x = pub_year, valence_mean))
q + geom_point() +
    # geom_line() +
    geom_smooth(method = "lm")

library(moderndive)
valence_wordcontext_lm <- lm(valence_mean ~ pub_year, data = lsd_wordcontext_year)
get_regression_table(valence_wordcontext_lm)

## VALENCE BY MONTH [WORD CONTEXT] ##########

lsd_wordcontext_df <- 
    lsd_wordcontext_df %>% 
    mutate(neg_pos_val = case_when(valence < 0 ~ 0,
                                   valence > 0 ~ 1,
                                   TRUE ~ NA_real_))
tabyl(lsd_wordcontext_df$neg_pos_val)               

lsd_wordcontext_df$neg_pos_val <- factor(lsd_wordcontext_df$neg_pos_val, 
                                         levels = c(0,1), 
                                         labels = c("negative", "positive"))

lsd_wordcontext_plot_day <- 
    lsd_wordcontext_df %>% 
    filter(valence != 0) %>% 
   # filter(source_pred == "Straits Times") %>% 
    ggplot(aes(x = pub_date, y = valence, color = neg_pos_val))

lsd_wordcontext_plot_day + 
    geom_point(aes(color = neg_pos_val)) +
    geom_smooth(method = "loess") + 
    scale_color_manual(values = c(blue,orange)) 

## Average valence comparing neg and positive valence groups ########
lsd_wordcontext_df %>% 
    filter(!is.na(neg_pos_val)) %>% 
    group_by(neg_pos_val) %>% 
    summarise(mean_valence = mean(valence))

valence <- 
    lsd_wordcontext_df  %>% 
    select(doc_id, valence, neg_pos_val, source) %>% 
    filter(!is.na(neg_pos_val)) 

valence$valence_abs <- abs(valence$valence)
valence_aver_lm <- lm(valence ~ neg_pos_val, data = valence)
get_regression_table(valence_aver_lm) 

## WORD CLOUD THE WORDS AROUND KEY PHRASE #############
    # i use the token wordcontext_phrase_tok_removed

head(wordcontext_phrase_tok_removed[2])

wordcontext_dfm <- 
    dfm(wordcontext_phrase_tok_removed,
        remove = stopwords('english'))

set.seed(234)
textplot_wordcloud(word_cloud_wordcontext_dfm,
                   min_count = 3,
                   color = c(lightblue, blue, orange))

ndoc(word_cloud_wordcontext_dfm)

textstat_frequency(word_cloud_wordcontext_dfm)



lsd_wordcontext_dfm <- dfm(wordcontext_lsd) 

lsd_wordcontext_df <- 
    convert(lsd_wordcontext_dfm, to = "data.frame") %>%
    cbind(docvars(lsd_wordcontext_dfm))