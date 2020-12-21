### Based on: https://slcladal.github.io/tagging.html

#Loading packages
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
packages = c("dplyr", "igraph", "tm", "NLP", "openNLP",
                  "openNLPdata", "coreNLP", "stringr", "koRpus", 
                  "koRpus.lang.en")
install.packages(packages)
install.packages("rJava")

#loading libraries
# activate packages
library(dplyr)
library(igraph)
library(tm)
library(NLP)
library(openNLP)
library(openNLPdata)

#solving the problem with rJava
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_271") # for 64-bit version
library(rJava)

#library(coreNLP)
library(stringr)
library(koRpus)
library(koRpus.lang.en)
# load function for pos-tagging objects in R
source("https://slcladal.github.io/rscripts/POStagObject.r")

# load corpus data
write.csv(iphone_reviews, "iphone_reviews.csv")
text <- read_csv("iphone_reviews.csv")

#load the function for pos-tagging
POStag <- function(object){
  require("stringr")
  require("NLP")
  require("openNLP")
  require("openNLPdata")
  # define paths to corpus files
  corpus.tmp <- object
  # define sentence annotator
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  # define word annotator
  word_token_annotator <- Maxent_Word_Token_Annotator()
  # define pos annotator
  pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en", probs = FALSE, 
                                                # WARNING: YOU NEED TO INCLUDE YOUR OWN PATH HERE!                                            
                                                model = "C:\\Users\\João\\Documents\\R\\win-library\\4.0\\openNLPdata\\models")
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
  # loop over file contents
  lapply(Corpus, function(x){
    y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    y2<- NLP::annotate(x, pos_tag_annotator, y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
}

# pos tagging data
textpos <- POStag(object = text)
textpos



