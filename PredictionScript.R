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
##Take 20,000 samples from each category.
set.seed(37)
DataSample<-c(sample(twitter, 30000), sample (blogs, 30000), sample(news,30000))

###########################################Precleaning###################################
##Create/clean corpus##
##Removes ASCII, etc.##
DataSample<-iconv(DataSample,"latin1","ASCII", sub="")

##Creates corpus using quanteda package.
#corpus<-quanteda::corpus(DataSample)
##Turns all characters to lowercase.
profanities <- readLines('profanity.txt')
StopWords<-stopwords()

corpus <- VCorpus(VectorSource(DataSample))
dataframe<-data.frame(text=unlist(sapply(corpus, "content")), stringsAsFactors=F)
dataframe$text<-tolower(dataframe$text)

for (i in 1:length(profanities)){
  dataframe$text<-gsub(profanities[i],"", dataframe$text)
}


dataframe$text <- gsub("[][?!#$%()*,.:;<=>@^_|~.{}]", "", dataframe$text)
dataframe$text <- gsub("\\\\", "", dataframe$text)
dataframe$text <- gsub('\"', "", dataframe$text, fixed=T)
corpus<-VCorpus(VectorSource(dataframe$text))


#Deleting all English stopwords and any stray letters left my the non-ASCII removal
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
#Cleaning all non ASCII characters
corpus <- tm_map(corpus,toSpace,"[^[:graph:]]")
#Transforming all data to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
#Deleting all English stopwords and any stray letters left my the non-ASCII removal
corpus <- tm_map(corpus,removeWords,c(stopwords("english"),letters))
#Removing Punctuation
corpus <- tm_map(corpus,removePunctuation)
#Removing Numbers
corpus <- tm_map(corpus,removeNumbers)

#Removing all stray letters left by the last two calls
corpus <- tm_map(corpus,removeWords,letters)
#Striping all extra whitespace
corpus <- tm_map(corpus,stripWhitespace)

corpus<-quanteda::corpus(corpus)


write.table(corpus, file="PredictionCorpus.txt")

p1<-sbo_predictor(corpus, dict=target~0.75, .preprocess = sbo::preprocess, EOS = ".?!:;", N=3)
predict(p1, "president")

summary(p1)


##Code for use on Shiny App##
corpus_trained<-sbo_predtable(object = corpus, N = 3, dict = target ~ 0.75, .preprocess = sbo::preprocess, 
                              EOS = ".?!:;")
save(corpus_trained, file="corpus_trained.rda")

load("corpus_trained.rda")
corpus_trained2<-sbo_predictor(corpus_trained)
predict(corpus_trained2, "dancing")
predictvector<-predict(corpus_trained2, "new york")
if (predictvector[1]=="<EOS>"){
  predictvector<-predictvector[2]
  
}
predictvector[1]

stopwords()
