# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# ANALYSIS OF TITLES


# LOAD DATAET
#############################

rm(list = ls())
getwd()
load(file = "an_data/an_swnews_cleaned.RData")
an_swnews_title <- an_swnews_cleaned 

library(tidyverse)
library(quanteda)

# title to POS
# ===========
library(phrasemachine)

# Rename dataset to an_swnews_title_pos
an_swnews_title_pos <- an_swnews_title

# POS cannot work on single word (though they have unigram)
an_swnews_title_pos$one_wrd <- stringi::stri_count(an_swnews_title_pos$title, regex="\\S+")

an_swnews_title_pos <- an_swnews_title_pos[!an_swnews_title_pos$one_wrd < 2, ]
table(an_swnews_title_pos$one_wrd)

#an_swnews_title_pos <-an_swnews_title_pos[!an_swnews_title_pos$id == 9,]
#an_swnews_title_pos <-an_swnews_title_pos[!an_swnews_title_pos$id == 925,]
#an_swnews_title$title[9]
#an_swnews_title$title[925]

#rm(an_swnews_cleaned)
#rm(an_swnews_title)
#an_swnews_title_pos_100articles <- an_swnews_title_pos[an_swnews_title_pos$id < 100,]


# title to corpus
# ==============
title_corpus <- corpus(an_swnews_title_pos, 
                       docid_field = "id",
                       text_field = "title"
                       )

phrasemachine(title_corpus,
              regex = "(A|N)*N(PD*(A|N)*N)*",
              maximum_ngram_length = 4,
              minimum_ngram_length = 2,
              memory = "-Xmx1G")

phrasemachine(an_swnews_title_pos$text,
              regex = "(A|N)*N(PD*(A|N)*N)*",
              maximum_ngram_length = 4,
              minimum_ngram_length = 2)



##############################################

# title to corpus
# ==============
title_corpus <- corpus(an_swnews, 
                       docid_field = "id",
                       text_field = "title" )

#    c("pub.year"))

# summary(title_corpus)
# summary(summary(title_corpus))

texts(title_corpus)[4]
title_corpus[1]
title_corpus$section_forum[1:4]
class(title_corpus)

#textplot_xray(kwic(corpus_subset(title_corpus, section_forum == 1), 
#     pattern = phrase("social work*")))


# KWIC
# ========
options(width = 120)
kwic(title_corpus, pattern = phrase("social work*"), 
     valuetype = "regex",
     window = 4)

# change corpus to DTM
# ============================
mystopwords <- c("social work", "social worker", "social workers", "social-work", "social-worker", "social-workers")
title_dtm <- dfm(title_corpus , 
                 tolower = T,
                 remove_numbers = T,
                 remove_punct = T,
                 remove_separators = T,
                 remove = c(phrase(mystopwords), stopwords()) , 
                 stem = F)
# i removed my own custom stop list and the standard stop list
# Note that summary statistcs of title_dtm will not include words excluded 

# see features (words) in title
head(title_dtm, n =3)
head(title_dtm@Dimnames$features , n =100)

str_view(an_swnews$title[150:200], pattern = "^(?!.*(social work)).*social.*$")
str_view(an_swnews$title, pattern = "social\\b")
# Even with my custom stoplist, there is still the word social 
# because "social problems", "social needs", "social service"

# word count of words in title
title_wordcount <- colSums(title_dtm)
title_wordcount = title_wordcount[order(title_wordcount, decreasing = T)]

head(title_wordcount, n =50)
tail(title_wordcount, n =100)
topfeatures(title_dtm)  

title_corpus[str_detect(title_corpus, pattern = "\\$" )]
#View(title_corpus[str_detect(title_corpus, pattern = "\\$" )])
# looking for titles with $ since this is one of the words is freq

as.character(title_corpus[1]) # this prints better than just title_corpus[1]

# Word cloud
# ================
set.seed(111)
textplot_wordcloud(title_dtm, 
                   #max_words = 100, 
                   min_count = 10,
                   random_order = F, 
                   #  rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))  #

str_view(an_swnews$title, pattern = "social\\b")
str_view(an_swnews$title, pattern = "pay\\b")
str_view(an_swnews$title[600:977], pattern = "life\\b")
title_corpus[str_detect(title_corpus, pattern = "\\$" )]


#library(regexplain)

# Convert to DFM again but by groups
# ===================================

title_forum_dtm <- dfm(title_corpus , groups = "section_forum",
                       tolower = T,
                       remove_numbers = T,
                       remove_punct = T,
                       remove_separators = T,
                       remove = c(phrase(mystopwords), stopwords()) , 
                       stem = F)



title_forum_word_count <- colSums(title_forum_dtm)
head(title_forum_word_count)
dfm_sort(title_forum_dtm)


# Sentiment analysis
# ==================
# Built-In Quanteda Dictionary: Lexicoder Sentiment Dictionary
# https://quanteda.io/reference/data_dictionary_LSD2015.html 
summary(data_dictionary_LSD2015)
data_dictionary_LSD2015$positive[300:500]
data_dictionary_LSD2015$negative[400:600]
data_dictionary_LSD2015$neg_positive[1:100] # flips the negative word
data_dictionary_LSD2015$neg_negative[1000:1100] # flip the positive word

# Applying a Dictionary
# ======================

#toks = tokens(title_corpus)
toks <- tokens_compound(tokens(title_corpus), data_dictionary_LSD2015)
# tokenized first. It says in the website that it is better to copound words first
# Not sure if we can do tokenization in the dfm command next
# After tokenizing, then change it to DFM

dfm_toks <- dfm(toks, 
                tolower = T,
                remove_numbers = T,
                remove_punct = T,
                remove_separators = T,
                remove = c(phrase(mystopwords), stopwords()),
                stem = F)
# change to DFM + remove my custom lust of words

dfm_lsd_coded <- dfm_lookup(dfm_toks,data_dictionary_LSD2015 )
# dfm_lookup links the words to the dictionary

head(dfm_lsd_coded, n = 3)

# Using the coded terms, we make a document_term matrix...then convert to dataframe

valences_by_title <- dfm_lsd_coded %>% 
    convert(to = "data.frame") %>% 
    cbind(docvars(dfm_lsd_coded)) 
# this last line keeps the docvars (meta id)
# so that i can analyze valence over time

# We may also want/need the total number of terms per document. 

head(dfm_toks) 
valences_by_title$total_words = rowSums(dfm_toks) # dfm_toks is the dfm of the text with pre-processing done


# Calculate Y&S measure: 
valences_by_title$valence = (valences_by_title$positive/valences_by_title$total_words) - 
    (valences_by_title$negative/valences_by_title$total_words)


#valences_by_title$valence = (valences_by_title$positive)  - (valences_by_title$negative)



# Look at valence over time:


valences_by_title_1999 <- valences_by_title %>% filter(pub_year != 1999)

# line graph
# 
p <- ggplot(valences_by_title, aes(x = pub_month,  y = valence)) 

p + geom_line(aes(group = pub_year)) + facet_wrap(~ pub_year)

# point

p + geom_point(color = "gray60", aes(group = pub_year)) + facet_wrap(~pub_year) 

# point 2 
p <- ggplot(valences_by_title, aes(x = reorder(pub_year, valence, na.rm = T) , y = valence))
p + geom_point()  + coord_flip() + labs(x = NULL)

p + geom_jitter()  + coord_flip() + labs(x = NULL)


# point (Cleveland dot plot)
by_year <- valences_by_title %>% group_by(pub_year) %>% 
    summarize(valence_mean = mean(valence, na.rm = T),
              valence_sd = sd(valence, na.rm = T)
    ) %>% 
    ungroup()

p <- ggplot(by_year, mapping = aes(x = valence_mean, y = pub_year ))
p + geom_point(size =3) + geom_vline(xintercept=0, linetype="dashed") 

by_year <- by_year %>% mutate(below0 = case_when(valence_mean <0   ~ 0,
                                                 valence_mean >= 0 ~ 1)) 
by_year$below0 <- factor(by_year$below0, labels = c("negative", "positive"))

p <- ggplot(by_year, mapping = aes(x = valence_mean, y = pub_year ))

p + geom_point(size =3, aes(color = below0)) + geom_vline(xintercept=0, linetype="dashed") +
    scale_colour_brewer(palette = "Set1", limits = c("negative", "positive") ) +
    geom_segment(aes(yend = pub_year), xend = 0, colour = "grey50") + theme_bw() +
    theme(panel.grid.major.y = element_blank()) 

year2001 <- corpus_subset(title_corpus, pub_year == 2001) 
year2000 <- corpus_subset(title_corpus, pub_year == 2000) 
year1999 <- corpus_subset(title_corpus, pub_year == 1999)
year1996 <- corpus_subset(title_corpus, pub_year == 1996)
year2020 <- corpus_subset(title_corpus, pub_year == 2020)

View(year2001)
View(year2000)
View(year1999)
View(year1996)
View(year2020)
table(an_swnews$pub_year)
# point (Cleveland dot plot) + FORUM

by_year_forum <- valences_by_title %>% group_by(pub_year, section_forum) %>% 
    summarize(valence_mean = mean(valence, na.rm = T),
              valence_sd = sd(valence, na.rm = T)
    ) %>% 
    ungroup()

p <- ggplot(by_year_forum, mapping = aes(x = valence_mean, y = pub_year ))
p + geom_point(size =3) + geom_vline(xintercept=0, linetype="dashed") + 
    facet_grid(section_forum ~ ., scales = "free_y", space = "free_y")

by_year_forum <- by_year_forum %>% mutate(below0 = case_when(valence_mean <0   ~ 0,
                                                             valence_mean >= 0 ~ 1)) 
by_year_forum$below0 <- factor(by_year_forum$below0, labels = c("negative", "positive"))

p <- ggplot(by_year_forum, mapping = aes(x = valence_mean, y = pub_year ))

p + geom_point(size =3, aes(color = below0)) + geom_vline(xintercept=0, linetype="dashed") +
    #  scale_colour_brewer(palette = "Set1", limits = c("negative", "positive") ) +
    geom_segment(aes(yend = pub_year), xend = 0, colour = "grey50") + theme_bw() +
    theme(panel.grid.major.y = element_blank()) +
    facet_grid(section_forum ~ ., scales = "free_y", space = "free_y")



# point (Cleveland dot plot) with SD
p <- ggplot(by_year, mapping = aes(x = pub_year, y =  valence_mean ))
p + geom_pointrange(aes(ymin = valence_mean - valence_sd, ymax = valence_mean + valence_sd )) + coord_flip()






lm.valences_by_title = lm(valence ~ as.numeric(pub_year) , data = valences_by_title)
summary(lm.valences_by_title)

