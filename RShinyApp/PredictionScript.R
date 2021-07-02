##Capstone Prediction Script##

##Libraries##
library(tidyverse)
library(knitr)
library(stringi)
library(ff)
library(quanteda)
library(quanteda.textplots)
library(sbo)
library(tm)

###########################################Loading Data###################################
##Build directories, downloads data, unzips data, and downloads a reference profanity 
##filter document.##

##Creates data folder.##
if(!file.exists("./data")){
  dir.create("./data")
}

##Downloads data zip file.##
if(!file.exists("Coursera-SwiftKey.zip")){
  Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(Url,destfile="Coursera-SwiftKey.zip",mode = "wb")
}

##Unzips data zip file.##
if(!file.exists("./data/final")){
  unzipFile<-file.path(getwd(),"Coursera-SwiftKey.zip")
  unzip(zipfile=unzipFile)
  ##Moves folder to data folder##
  path1 <- file.path(getwd(),"final")
  path2 <- file.path(file.path(getwd(),"data"),"final")
  file.move(path1,path2)
}


##Downloads a profanity file to subset against##
if (!file.exists("profanity.txt")) {
  profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  download.file(profanity.url, destfile = "profanity.txt")
}


##Read files into R##
twitter<-readLines("./data/final/en_US/en_US.twitter.txt",warn=FALSE,encoding="UTF-8")
blogs<-readLines("./data/final/en_US/en_US.blogs.txt",warn=FALSE,encoding="UTF-8")
news<-readLines("./data/final/en_US/en_US.news.txt",warn=FALSE,encoding="UTF-8")

###########################################Sampling the data###################################
##Take samples from each category.
set.seed(37)
DataSample<-c(sample(twitter, 35000), sample (blogs, 35000), sample(news,35000))
DataSample<-iconv(DataSample,"latin1","ASCII", sub="")
###########################################Precleaning###################################
##Reads profanity file##
profanities <- readLines('profanity.txt')
##Creates intial corpus and transforms to data frame for initial cleaning.##
corpus <- VCorpus(VectorSource(DataSample))
dataframe<-data.frame(text=unlist(sapply(corpus, "content")), stringsAsFactors=F)
dataframe$text<-tolower(dataframe$text) ##Changes corpus to lower case.##

##Filters profanities##
for (i in 1:length(profanities)){
  dataframe$text<-gsub(profanities[i],"", dataframe$text)
}

##Filters out special characters and common numbers.##
dataframe$text <- gsub("[][?!#$%()*,.:;<=>@^_|~.{}]", "", dataframe$text)
dataframe$text <- gsub("\\\\", "", dataframe$text)
dataframe$text <- gsub('\"', "", dataframe$text, fixed=T)
dataframe$text <- gsub('one', "", dataframe$text)
dataframe$text <- gsub('two', "", dataframe$text)
dataframe$text <- gsub('three', "", dataframe$text)
dataframe$text <- gsub('four', "", dataframe$text)
dataframe$text <- gsub('five', "", dataframe$text)
corpus<-VCorpus(VectorSource(dataframe$text))


#Deleting all English stopwords and any stray letters left my the non-ASCII removal
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
#Cleaning all non ASCII characters
corpus <- tm_map(corpus,toSpace,"[^[:graph:]]")
#Transforming all data to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
#Deleting all English stopwords and any stray letters
corpus <- tm_map(corpus,removeWords,c(stopwords("english"),letters))
#Removing Punctuation
corpus <- tm_map(corpus,removePunctuation)
#Removing Numbers
corpus <- tm_map(corpus,removeNumbers)
#Removing all stray letters
corpus <- tm_map(corpus,removeWords,letters)
#Striping all extra whitespace
corpus <- tm_map(corpus,stripWhitespace)

##Converts corpus to a quanteda corpus for use in the SBO package.##
corpus<-quanteda::corpus(corpus)


##Creates prediction model for the corpus using the SBO package.##
p1<-sbo_predictor(corpus, dict=target~0.75, .preprocess = sbo::preprocess, EOS = ".?!:;", N=3)
predict(p1, "mail")
summary(p1)


##Saves corpus as an .rda file for use on Shiny App.##
corpus_trained<-sbo_predtable(object = corpus, N = 3, dict = target ~ 0.75, .preprocess = sbo::preprocess, 
                              EOS = ".?!:;")
save(corpus_trained, file="corpus_trained.rda")

##Code for use on Shiny App##
load("corpus_trained.rda")
corpus_trained2<-sbo_predictor(corpus_trained)
predict(corpus_trained2, "dancing")
predictvector<-predict(corpus_trained2, "new york city")
if (predictvector[1]=="<EOS>"&&predictvector[2]!="said"&&predictvector[3]!="will"){
  predictvector[1]<-predictvector[2]
  
}
if (predictvector[1]=="<EOS>"&&predictvector[2]!="said"&&predictvector[3]!="will"){
  predictvector[1]<-predictvector[3]
  
}

if (predictvector[1]=="<EOS>"&&predictvector[2]!="said"&&predictvector[3]!="will"){
  predictvector[1]<-"No prediction available."
  
}

if (predictvector[1]=="<EOS>"&&predictvector[2]=="said"&&predictvector[3]=="will"){
  predictvector[1]<-"said"
  
}

predictvector[1]
