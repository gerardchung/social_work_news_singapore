---
output:
  pdf_document: default
  html_document: default
---

# Text-mining news on Social Work Profession in Singapore :singapore:

**README WORK IN PROGRESS**

## Data-collection
I searched for news articles in Nexis Uni (then it was called Lexis-Nexis Academic)

Search date was on 29th Jan 2020

A total of 7167 articles downloaded in docx format

**Search parameters**

 (1) Date of articles: all date range
 
 (2) Language: English
 
 (3) Date: All available dates 
 
 (4) Sources are the three newspapers in Singapore:
 
        - [Channel NewsAsia](https://www.channelnewsasia.com/news/singapore)
        
        - [Today (Singapore) – Online](https://www.todayonline.com/)
        
        - [The Straits Times (Singapore)](https://www.straitstimes.com/global)
        
        - [The Business Times (Singapore)](https://www.businesstimes.com.sg/)
        
 (5) Search string: "(social work) OR (social worker\*) OR (social-work) OR (social-worker\*)"
 
 (6) Search fields: “All fields” (i.e., including title, leading section, body etc)

**Results of search**: 7167 articles spanning year 1992-2021

***Note: Lexis University's policies do not allow me to share the data***

## Data-preprocessing

### Cleaning 
Data needs to be checked for duplicates. If downloads are made from Lexis Academic, 
the downloading process may include hidden files in the folders. This will create problems
when using the codes (command filelist) to create file names and file pathways. 


**Task List**

- [x] Data-collection
- [x] Training model
- [x] Cleaning data
- [ ] Descriptives
- [ ] Analysis 