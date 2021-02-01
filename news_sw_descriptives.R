# TEXT-MING SOCIAL WORK NEWS 
# ==========================

# After finalizing the dataset in "predicting", i now:
# (1) clean
# (2) do some descriptives
# N = 5168 

# SECTION A. PREPARE THE VARIABLES
## Code up the predictors

Packages <- c("dplyr", "ggplot2",  "ggplot2", "stringr", "xlsx")
lapply(Packages, library, character.only = TRUE)
text_packages <- c("quanteda", "tidytext")
lapply(text_packages, library, character.only = TRUE)

# LOAD DATASET
#############

rm(list = ls())
load(file = "an_data/an_swnews.Rdata") 

# CLEANING
##########


# pub.date 
# ========
    # Extract year, month, and day from pub.data

head(unique(an_swnews$pub.date), n = 40)
tail(unique(an_swnews$pub.date), n = 40)

# month
months <- c("January", "February", "March", "April", 
           "May", "June", "July", "August", 
           "September", "October", "November", "December")

for (i in months) {
    an_swnews$pub_month <- str_extract(an_swnews$pub.date, 
                                       regex(pattern = paste(months, collapse = "|"), ignore_case = T))
}

head(an_swnews$pub_month, n = 10)
head(an_swnews$pub.date, n = 10)
table(an_swnews$pub_month)

an_swnews$pub_month <- factor(an_swnews$pub_month, levels = months)
janitor::tabyl(an_swnews$pub_month)

# year 
head(an_swnews$pub.date, n = 50)
str_view(an_swnews$pub.date, pattern = "\\d{4}")

an_swnews$pub_year <- str_extract(an_swnews$pub.date, pattern = "\\d{4}")
head(an_swnews$pub_year, n =10)
table(an_swnews$pub_year)


# day
head(an_swnews$pub.date, n = 50)
str_view(an_swnews$pub.date, pattern = "\\d{1,2}")
str_view(an_swnews$pub.date, pattern = "\\d+") # this also work

an_swnews$pub_day <- str_extract(an_swnews$pub.date, pattern = "\\d{1,2}")
an_swnews$pub_day <- as.numeric(an_swnews$pub_day)
table(an_swnews$pub_day)

View_date <- select(an_swnews,starts_with("pub") )

# Check on day variable [detect if errors for e.g. day > 31 days]
for (value in an_swnews$pub_day) {
    if (value > 31) {
        print(str_c(an_swnews$id ," has", " problem!"))
    } else {
        print("No problem, move on!")
    }
}

table(an_swnews$pub_day, exclude = F)
table(an_swnews$pub_year,exclude = F)
table(an_swnews$pub_month, exclude = F)

# Convert to DATA using lubricate
date.lub <- c("pub_year","pub_month", "pub_day")
an_swnews$pub.date <- lubridate::ymd(paste(an_swnews$pub_year, an_swnews$pub_month,an_swnews$pub_day))
class(an_swnews$pub.date)

an_swnews <- an_swnews %>% relocate(starts_with("pub_"), .after = pub.date)

an_swnews %>% ggplot(aes(pub.date)) + geom_freqpoly(binwidth = 30) # R for data sci book pp.241

# Do a summarize var for num of articles by year published for later plotting
year_articles <- an_swnews %>% 
    group_by(pub_year) %>% 
    summarize(num_articles = n())


# section
#########
    # see if can distinguish forum section
    # In today news, it is called "voices"

unique(an_swnews$section)

an_swnews$voice <- str_detect(an_swnews$section, regex(pattern = "voice", ignore_case =T))
sum(an_swnews$voice, na.rm = T) # n =2
filter(an_swnews, voice == T)


# detect for pattern in section with either word "forum" or "letter"
an_swnews$section_forum <- str_detect(an_swnews$section, regex(pattern = "forum|letter|voice", ignore_case =T))
an_swnews$section_forum <-    as.numeric(an_swnews$section_forum) # change from logical to numeri

table(an_swnews$section_forum, exclude = F)

# detect for a new pattern in text since "i refer" and "i commend" in text => forum letters
pattern.refer.commend = "I commend|We commend|I refer|We refer|with reference"
section1 <- an_swnews[str_detect(an_swnews$text, regex(pattern = pattern.refer.commend, ignore_case =T)),]
View_section1 <- section1 %>% select(text, section, section_forum)
    # we can replace the empty values with T or 1 if there is this pattern in the text
    # note that section = Voice can also be a forum letter
    # six articles are detected using this new pattern

forum_pattern1 <- str_detect(an_swnews$text, regex(pattern = pattern.refer.commend, ignore_case =T))
table(an_swnews$section_forum, exclude = F)

an_swnews <- an_swnews %>% 
    mutate(section_forum = replace(section_forum, forum_pattern1 == T, 1))

table(an_swnews$section_forum, exclude = F) # 96 are forum/ letters

table(an_swnews$section_forum)
an_swnews$section_forum <- factor(an_swnews$section_forum, labels = c("articles", "forum letters"))

# Source
######## 
unique(an_swnews$source)
an_swnews <- an_swnews %>% mutate(source = replace(source, source == ("Today (Singapore) - Online") , "Today"))
an_swnews <- an_swnews %>% mutate(source = replace(source, source == ("TODAY (Singapore)") , "Today"))         

an_swnews$source <-  replace(an_swnews$source, an_swnews$source =="The Straits Times (Singapore)" , "Straits Times")
an_swnews$source <-  replace(an_swnews$source, an_swnews$source =="The Straits Times" , "Straits Times")

unique(an_swnews$source)
an_swnews$source <- factor(an_swnews$source)
table(an_swnews$source)

# title 
########
head(an_swnews$title)

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


################ # article by year

year_articles_source <- an_swnews %>% 
  group_by(pub_year, source) %>% 
  summarize(num_articles = n()) %>% 
  ungroup()
lm.year_articles = glm(num_articles~as.numeric(pub_year), family="poisson", data = year_articles_source)

lm.year_articles = glm(num_articles~as.numeric(pub_year)*source, family="poisson", data = year_articles_source)
summary(lm.year_articles)

p <- ggplot(data = year_articles_source, mapping = aes(x = as.numeric(pub_year),y = num_articles)) +
  geom_line(color = "gray60", size = 1, aes(group = source) ) + 
  geom_smooth(method = "lm", se = FALSE, aes(group = source)) +
  labs(x = "Year", y = "Number of articles") +
  ggtitle("Number of articles to Year 1994-2020") 
p
 
# lm.year_articles = glm(num_articles~as.numeric(pub_year)*source, family="poisson", data = year_articles)
# summary(lm.year_articles)
# year_articles$pub_year <- as.numeric(year_articles$pub_year)
# library(gganimate)
 p <- ggplot(data = year_articles, mapping = aes(x = (pub_year),y = num_articles)) +
     geom_line(color = "gray60", size = 1 ) + 
     geom_smooth(method = "loess", se = FALSE) +
     labs(x = "Year", y = "Number of articles") +
     ggtitle("Number of articles to Year 1994-2020") 
 
# q <- ggplot(data = year_articles, mapping = aes(x = (pub_year),y = num_articles)) +
#         geom_line(color = "#56B4E9", size = 1 ) + geom_point() +
#         labs(x = "Year", 
#              y = "Number of articles",
#              title = "Number of articles about Social Work Profession \npublished in Singapore newspapers from Year 1994-2020",
#              subtitle = "Straits times, CNA, and Today") +
#         theme(plot.title = element_text(color = "red", size = 10, face = "bold"))  +
#         scale_x_continuous( breaks=seq(1992, 2020, 4)) +
#         hrbrthemes::theme_ipsum() 
# q
# 
# 
# q_animate <- q + transition_reveal(pub_year) + enter_fade()
# getwd()
# anim_save("num_article_years.gif", animation = q_animate)


# 
# hrbrthemes::import_roboto_condensed() 
# library(hrbrthemes)
# p + geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=1) +
#     geom_smooth(method = "lm", se = FALSE) +
#     theme_ipsum() +
#     labs(x = "Year", y = "Number of articles") +
#     ggtitle("Number of articles to Year 1992-2020")

