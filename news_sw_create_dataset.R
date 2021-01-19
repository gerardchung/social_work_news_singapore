# TEXT-MING SOCIAL WORK NEWS 
# ==========================

rm(list = ls())

library(textreadr)
library(stringr)
library(dplyr)
getwd()
# First, let's create a list of the files, which is the same first step we've seen before. However, here we can just use your computer's file structures and R commands to do so. 

# Extract file names
file.list = list.files("data", recursive = T) # recursive will perform in each directory 
file.list = str_subset(file.list, pattern = "doc_list", negate = T)
sum(str_detect(file.list, pattern = "doc_list")) # check if still have
sum(str_detect(file.list, pattern = "_doclist")) # 1345 values because there is this unknown "_$_doclist" value

file.list = str_subset(file.list, pattern = "_doclist", negate = T ) # remove this last value

sum(str_detect(file.list, pattern = "_doclist")) # check if still have
sum(str_detect(file.list, pattern = "doc_list"))


# folder number 
file.list[1]
str_view(file.list, "^\\d{1,2}") # digit class from 1 to 2 digits
foldernum <- str_extract(file.list, "^\\d{1,2}")
summary(as.numeric(foldernum))



# Let's make those full file paths that can be used to download the files.
file.list
file.list2 = paste("data",file.list, sep="/") 
                # data because /data directory
                # change if the directory is different
                # paste data + file.list, separated by /
                # e.g., data + / + 1/Desire to serve draws more t....docx 

#file.list2[1:2]

    # trial with one document
    # ======================
    trial = read_docx(file.list2[1])
    trial
    trial = read_docx(file.list2[3])
    trial
    #  text body
        # see that the body of news starts at line after "Body: and end at line before "Classification"
    start.text = which(trial == "Body") + 1
    end.text   = which(trial == "Classification") - 1
   
    text = paste(trial[start.text:end.text], collapse = "\n")
    cat(text, "\n")

    # Next, let's grab each of the options that has an explicit tag.

    (section = gsub("Section:","",trial[grepl("Section:",trial,fixed=T)] ,fixed=T))
            # Same as abv -> 
    (section = str_replace(trial[str_detect(trial, "Section:")], pattern = "Section:", replacement =""))
            # Need to first detect because the trial is in a vector
    (words = gsub("Length:","",trial[grepl("Length:",trial,fixed=T)] ,fixed=T))
    (language = gsub("Language:","",trial[grepl("Language:",trial,fixed=T)],fixed=T))
    
    (type = str_replace(trial[str_detect(trial, "Publication-Type:")], pattern = "Publication-Type:", replacement =""))
        # trial[1] has a section on "Publication-Type:" 
        # trial[3] does not have. So if just run codes withOUT ifelse, the type vector will have no data.
        # This will become a problem in the loop later (the loop will stop because entering NO data into the dataframe)
        # This ifelse code => if length is > 0, then ran the gsub. Else, input NA 
    (type = ifelse(length(trial[grepl("Publication-Type:",trial,fixed=T)]) >0,
                  gsub("Publication-Type:","",trial[grepl("Publication-Type:",trial,fixed=T)],fixed=T),
                  NA))
    (subject = gsub("Subject:","",trial[grepl("Subject:",trial,fixed=T)],fixed=T))
    (industry = gsub("Industry:","",trial[grepl("Industry:",trial,fixed=T)],fixed=T))
    (geographic = gsub("Geographic:","",trial[grepl("Geographic:",trial,fixed=T)],fixed=T))
   # (load.date = gsub("Load-Date:","",trial[grepl("Load-Date:",trial,fixed=T)],fixed=T))
   
    # These below are relational -> they should be the same position in every docu
    (title = trial[1])
    (source = trial[2])
    (pub.date = trial[3])
    
    # the abv codes with one doc show that we can do these to get info
    # the below begins to write codes with loop to get same info from ALL docs

    
# Now that we know how to extract all the information we can kick it up a notch and write a loop. 
    
# Step 1 here is to create the empty data frame.

sw.news <- data.frame( title = rep(NA, length(file.list2)),
                       source = rep(NA, length(file.list2)),
                       pub.date = rep(NA, length(file.list2)),
                       section = rep(NA, length(file.list2)),
                       words = rep(NA, length(file.list2)),
                       language = rep(NA,length(file.list2)),
                       type = rep(NA,length(file.list2)),
                       subject = rep(NA,length(file.list2)),
                       industry = rep(NA,length(file.list2)),
                       geographic = rep(NA,length(file.list2)),
                       text = rep(NA,length(file.list2)),
                       stringsAsFactors = F
                       )
    

# Step 2 is to create the loop by copying down the code we know extracts what we want and has it input it into our data frame.
for(i in 1:length(file.list2)) {
    print(paste("Working on document", i, "of", length(file.list2)))
    temp.doc = read_docx(file.list2[i])
    
    sw.news$title[i] = temp.doc[1]
    sw.news$source[i] = temp.doc[2]
    sw.news$pub.date[i] = temp.doc[3]
    
    #sw.news$section[i] = gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T)
        # there are article(s) that do not have sections -> trial[8]
    sw.news$section[i] = ifelse(length(temp.doc[grepl("Section:",temp.doc,fixed=T)]) >0,
                                gsub("Section:","",temp.doc[grepl("Section:",temp.doc,fixed=T)] ,fixed=T),
                                NA)
    sw.news$words[i] = gsub("Length:","",temp.doc[grepl("Length:",temp.doc,fixed=T)] ,fixed=T)
    sw.news$language[i] = gsub("Language:","",temp.doc[grepl("Language:",temp.doc,fixed=T)] ,fixed=T)
    
   # sw.news$type[i] = gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T)
        # there are article(s) that do not have sections -> trial[3]    
    sw.news$type[i] = ifelse(length(temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)]) >0, 
                                    gsub("Publication-Type:","",temp.doc[grepl("Publication-Type:",temp.doc,fixed=T)] ,fixed=T),
                                    NA)
    
    #sw.news$subject[i] = gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T)
    sw.news$subject[i] = ifelse(length(temp.doc[grepl("Subject:",temp.doc,fixed=T)] >0),
                                gsub("Subject:","",temp.doc[grepl("Subject:",temp.doc,fixed=T)] ,fixed=T),
                                NA)

    #sw.news$industry[i] = gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T)
        # at least one article 4 does not have industry
    sw.news$industry[i] = ifelse(length(temp.doc[grepl("Industry:",temp.doc,fixed=T)] >0),
                                 gsub("Industry:","",temp.doc[grepl("Industry:",temp.doc,fixed=T)] ,fixed=T),
                                 NA)
    
    # sw.news$geographic[i] = gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T)
    sw.news$geographic[i] = ifelse(length(temp.doc[grepl("Geographic:",temp.doc,fixed=T)] >0),
                                   gsub("Geographic:","",temp.doc[grepl("Geographic:",temp.doc,fixed=T)] ,fixed=T),
                                   NA)
       
    start.text = which(temp.doc == "Body") + 1
    end.text   = which(temp.doc == "Classification") - 1
    
    sw.news$text[i] = paste(temp.doc[start.text:end.text], collapse = "\n")
}

# Add in the vars that denote folder number
sw.news$foldernum <- foldernum
dplyr::glimpse(sw.news)

sw.news <- sw.news %>% 
    relocate(foldernum, before = title )



# save as Rdata file
# ====================
getwd()
save(sw.news, file = "cr_data/sw_news.RData") 



