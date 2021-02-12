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
an_swnews$pub_date <- lubridate::ymd(paste(an_swnews$pub_year, an_swnews$pub_month,an_swnews$pub_day))
class(an_swnews$pub_date)

an_swnews <- an_swnews %>% 
              relocate(starts_with("pub_"), .after = source) %>% 
              select(-(pub.date))
          

an_swnews %>% ggplot(aes(pub_date)) + geom_freqpoly(binwidth = 30) # R for data sci book pp.241

# Do a summarize var for num of articles by year published for later plotting
year_articles <- an_swnews %>% 
    group_by(pub_year) %>% 
    summarize(num_articles = n())


# section
#########
    # see if can distinguish forum section
    # In today news, it is called "voices"

unique(an_swnews$section)


an_swnews$voice <- str_detect(an_swnews$section, regex(pattern = "voice|view|opinion", ignore_case =T))
sum(an_swnews$voice, na.rm = T) # n =2
view_voice <- filter(an_swnews, voice == T)
view_voice[2] # Only "voice" are forum letters

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

an_swnews$source <-  replace(an_swnews$source, an_swnews$source =="Business Times (Singapore)" , "Business Times")
an_swnews$source <-  replace(an_swnews$source, an_swnews$source =="The Business Times Singapore" , "Business Times")


unique(an_swnews$source)
an_swnews$source <- factor(an_swnews$source)
table(an_swnews$source)

# title 
########
head(an_swnews$title)



# Save for any future analyses
##################################
an_swnews_cleaned <- an_swnews
save(an_swnews_cleaned, file = "an_data/an_swnews_cleaned.RData")


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

