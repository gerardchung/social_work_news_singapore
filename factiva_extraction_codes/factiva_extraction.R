
rm(list=ls())
library(tm.plugin.factiva)
library(tm)
library(quanteda)
#library(R.temis)

getwd()


source <- FactivaSource("source_data_factiva/Factiva1.html")
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


