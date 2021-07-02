##Capstone Milestone Report Script##

##Libraries##
library(tidyverse)
library(knitr)
library(stringi)
library(ff)
library(quanteda)
library(quanteda.textplots)

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




###########################################Summary Statistics###################################
##Word counts, line counts and basic data tables##
##Word counts:##
words_twitter<-stri_stats_latex(twitter)[4]
words_blogs<-stri_stats_latex(blogs)[4]
words_news<-stri_stats_latex(news)[4]

##Line counts:##
lines_twitter<-length(twitter)
lines_blogs<-length(blogs)
lines_news<-length(news)

data.frame("Source"=c("Twitter", "Blogs", "News"),
           "Number.of.Words"=c(words_twitter, words_blogs, words_news),
           "Number.of.Lines"=c(lines_twitter, lines_blogs, lines_news))


###########################################Sampling the data###################################
##Take 20,000 samples from each category.
set.seed(37)
DataSample<-c(sample(twitter, 20000), sample (blogs, 20000), sample(news,20000))

###########################################Precleaning###################################
##Create/clean corpus##
##Removes ASCII, etc.##
DataSample<-iconv(DataSample,"latin1","ASCII", sub="")

##Creates corpus using quanteda package.
corpus<-quanteda::corpus(DataSample)
##Turns all characters to lowercase.
corpus<-corpus%>%char_tolower()


###########################################Tokenization and further cleaning###################################

##Removes numbers, symbols, punctuation, symbols, and URLs.##
token <- tokens(corpus, what = "word",
              remove_numbers = TRUE, remove_punct = TRUE,
              remove_symbols = TRUE,
              remove_url = TRUE)

##Profanity filter
profanityFilter<-readLines("profanity.txt")
token<-tokens_remove(token, pattern=profanityFilter)

token<-tokens_remove(token, stopwords("english"))

##Create DFM (used in the 1-gram)
dfm<-dfm(token)

###########################################Exploratory Analysis###################################
##Create N-gram textplots.

##1-gram
textplot_wordcloud(dfm, min_size = 0.5, max_size = 4, max_words = 20)

##2-gram
token2<-tokens_ngrams(token, n=2L, concatenator = " ",)
token2<-tokens_remove(token2, stopwords("english"))
dfm2<-dfm(token2)
textplot_wordcloud(dfm2, max_words = 20)

#3-gram
token3<-tokens_ngrams(token, n=3L, concatenator = " ",)
token3<-tokens_remove(token3, stopwords("english"))
dfm3<-dfm(token3)
textplot_wordcloud(dfm3, max_words = 20)

##Create frequency bar plots.

##1-gram frequency)
onegramFrequency<-as.data.frame(topfeatures(dfm))
onegramFrequency<-tibble::rownames_to_column(onegramFrequency)  ##Changes row names to a different column
onegramFrequency<-rename(onegramFrequency, Word=rowname)
onegramFrequency<-rename(onegramFrequency, Number="topfeatures(dfm)")
ggplot(onegramFrequency, aes(x=reorder(Word,-Number), y=Number)) +geom_bar(stat="identity")+xlab("Word")+ylab("Frequency")+ggtitle("Frequency of Words") +theme(axis.text.x=element_text(angle=45))

##2-gram frequency
twogramFrequency<-as.data.frame(topfeatures(dfm2))
twogramFrequency<-tibble::rownames_to_column(twogramFrequency) ##Changes row names to a different column
twogramFrequency<-rename(twogramFrequency, Phrase=rowname)
twogramFrequency<-rename(twogramFrequency, Number="topfeatures(dfm2)")
ggplot(twogramFrequency, aes(x=reorder(Phrase,-Number), y=Number)) +geom_bar(stat="identity")+xlab("Phrase")+ylab("Frequency")+ggtitle("Frequency of Two-Word Phrases") + theme(axis.text.x=element_text(angle=45))

##3-gram frequency
threegramFrequency<-as.data.frame(topfeatures(dfm3))
threegramFrequency<-tibble::rownames_to_column(threegramFrequency) ##Changes row names to a different column
threegramFrequency<-rename(threegramFrequency, Phrase=rowname)
threegramFrequency<-rename(threegramFrequency, Number="topfeatures(dfm3)")
ggplot(threegramFrequency, aes(x=reorder(Phrase,-Number), y=Number)) +geom_bar(stat="identity")+xlab("Phrase")+ylab("Frequency")+ggtitle("Frequency of Three-Word Phrases") + theme(axis.text.x=element_text(angle=45))