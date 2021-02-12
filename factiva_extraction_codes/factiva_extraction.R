# TEXT-MING SOCIAL WORK NEWS 
# ==========================

rm(list=ls())

library(tm.plugin.factiva)
library(tm)
library(quanteda)
#library(R.temis)
library(textreadr)
library(stringr)
library(dplyr)

getwd()


# What I basically want to do with the loop later:
    # Using FactivaSource and Corpus from TM package and tm.plugin.factiva package, 
    # this will extract the html files and conerts it into Vcorpus files
    # quanteda::corpus(v.corpus) converts Vcorpus to corpus
    # convert(corp_quanteda , to = c("data.frame"), pretty = FALSE) will convert corpus to dataframe
source <- FactivaSource("source_data_factiva/factiva_raw/factiva_2000/Factiva1.html")
v.corpus <- Corpus(source, readerControl = list(language = NA))
inspect(v.corpus[1])
meta(v.corpus[[1]])
v.corpus[[1]][1] # [[1]] = first document; [1]  will get at the content 
v.corpus[[1]][2] # [[1]] = first document; [2]  will get at the metadata
# Vcorpus created by TM is a list of lists
# http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html
corp_quanteda <- quanteda::corpus(v.corpus) 
# converts Vcorpus to corpus
# meta data is also automatically converted
summary(corp_quanteda)
corp_df <- convert(corp_quanteda , to = c("data.frame"), pretty = FALSE)


# the loop function
############################
    # The loops aims to load the raw html files from each folder
    # and then converts them from Vcorpus => Corpus => Dataframe
    # And then saves in the extracted_factiva folder using same names as the raw files

# Step 1: create the directory paths to load the raw files
# ========================================================
# First, let's create a list of the files, which is the same first step we've seen before. However, here we can just use your computer's file structures and R commands to do so. 

# Extract file directory names
file.list = list.files("source_data_factiva/factiva_raw", pattern = "\\D", recursive = T) # recursive will perform in each directory 

# Let's make those full file paths that can be used to download the raw files in each folder. 
    # for e.g. in folder 'factiva_2000" -> 3 files Factiva1.html, Factiva2.html, Factiva3.html
file.list
file.list2 = paste("source_data_factiva/factiva_raw",file.list, sep="/") 
file.list2[1] # "source_data_factiva/factiva_raw/factiva_2000/Factiva1.html"
        # source_data because /source_data
        # change if the directory is different
        # paste source_data + file.list, separated by /
        # e.g., source_data + / + 1/Desire to serve draws more t....docx 

# Extract file names 
    # After converting from html -> Vcorpus -> corpus -> dataframe, I will save the individual dataframes
    # This next step is thus to create file names for each dataframes 
    # So i am trying to extract the file name from each file based on their original raw file names 
    # (e.g., Factiva1.html -> extract "Factiva1")

file.list[1] # I want the string phrase matching "Factiva1" 

str_view(file.list, regex(pattern = "Factiva\\d+", ignore_case = F)) # "Factiva\\d+" is the pattern with ignore_case = Trye 
file.list.names <- str_extract(file.list, regex(pattern = "Factiva\\d+", ignore_case = F)) # extract the name of the raw files
file.list.names[1]  # "Factiva1" extracted - this will be the name of the dataframe

file.list.names1 <- paste("source_data_factiva/extracted_factiva",  file.list.names, sep = "/") 
    # this is the directory to save the dataframe. I will save it in extracted_factiva folder
file.list.names1[1]
    #  "source_data_factiva/extracted_factiva/Factiva1"
file.list.names2 <- paste(file.list.names1, ".RData", sep = "")
    # the file saved will be RData
file.list.names2[1] # "source_data_factiva/extracted_factiva/Factiva1.RData"
name_corp_df <-   paste(file.list.names)
file.list.names[1]
name_corp_df[1]

# Creating the loop function
# Loop to save each raw file into a dataframe
    # Step 1: length(file.list2) = the number of files 
    # Step 2: This is the notification for the progress of extraction
    # step 3: These next two lines are to extract the raw files using directories in file.list2
    # Step 4: corp_quanteda <- quanteda::corpus(v.corpus) -> Converts it from Vcorpus to corpus
    # Step 5: convert(corp_quanteda -> Convert it to dataframe
    # Step 6: paste(file.list.names[i]) <- paste the names into ab object
    # Step 7: assign the new name using name_corp_df to the corp_df 
    # Step 8: save the file as RData using name from  file.list.names2 and the object based on the list name_corp_df
    # Step 9: remove the data frame [I do not want so many DF opened in my com]

for(i in 1:length(file.list2)) {
    print(paste("Working on document", i, "of", length(file.list2)))
    source <- FactivaSource(file.list2[i])
    v.corpus <- Corpus(source, readerControl = list(language = NA))
    corp_quanteda <- quanteda::corpus(v.corpus) 
    corp_df <- convert(corp_quanteda , to = c("data.frame"), pretty = FALSE)
    name_corp_df <- paste(file.list.names[i])
    assign(name_corp_df, corp_df )
    save(file = file.list.names2[i], list=name_corp_df)
    rm(list = name_corp_df)
}
    # I will basically have every single raw HTML now converted and saved 
    # using the same name (its object saved is also of same name) 
    # i.e. Factiva1.RData (represents year 2000, factiva1.html) and its object is also Factiva1

rm(list=ls())
load("source_data_factiva/extracted_factiva/Factiva1.RData")
rm(list=ls())

# Load and append the dataframes 
#################################
    # https://lieselspangler.com/2017/11/16/how-to-load-and-append-multiple-files-in-r/
    # https://stackoverflow.com/questions/59966478/r-efficiently-bind-rows-over-many-dataframes-stored-on-harddrive
rm(list=ls())

getwd()
library(stringr)
library(dplyr)

files.list <- list.files(path = "source_data_factiva/extracted_factiva", pattern = "Factiva[1-6].RData")
file.list2 = paste("source_data_factiva/extracted_factiva",files.list, sep="/") 

data_list <- lapply(file.list2, function(f) {
    message("loading file: ", f)
    name <- load(f)                    # this should capture the name of the loaded object
    return(eval(parse(text = name)))   # returns the object with the name saved in `name`
})
factiva_df <- data.table::rbindlist(data_list, idcol = T) # idcol creates .id which is the number for Factiva1, Factiva2.....
factiva_df <- factiva_df %>%  rename (file_id  = .id)

rm(file.list2, files.list, data_list)