##Capstone Milestone Report Script##

##Libraries##
library(tidyverse)
library(knitr)
library(stringi)
library(ff)
##Build directories and unzip data.##
if(!file.exists("./data")){
  dir.create("./data")
}

if(!file.exists("./data/final")){
  unzipFile<-file.path(getwd(),"Coursera-SwiftKey.zip")
  unzip(zipfile=unzipFile)
  ##Moves folder to data folder##
  path1 <- file.path(getwd(),"final")
  path2 <- file.path(file.path(getwd(),"data"),"final")
  file.move(path1,path2)
}

##Read files into R##
twitter<-readLines("./data/final/en_US/en_US.twitter.txt",warn=FALSE,encoding="UTF-8")
blogs<-readLines("./data/final/en_US/en_US.blogs.txt",warn=FALSE,encoding="UTF-8")
news<-readLines("./data/final/en_US/en_US.news.txt",warn=FALSE,encoding="UTF-8")


##Word counts, line counts and basic data tables##
##Word counts:##
