#########################################################################
# Copyright (c) 2014 All Rights Reserved, Scott Alexander Malec
#
# This source is free to use and distribute so long as credit is provided.
# This code is provided "AS IS" without warranty of any kind, either
# expressed or implied, including but not limited to the implied
# warranties of merchantability and/or fitness for a particular purpose.
#
# Author: Scott Alexander Malec
# Email: scott [dot] malec [at] gmail [dot] com
# Date: 2/5/2014
#
# TITLE: SOTUS_topicmodels_all.R
#
# Purpose: create topicmodel from SOTUS corpus, compare&contrast topicmodels
#           of different eras/parties/etc.
#
#########################################################################

library("tm")
library("rJava")
library("RWeka")
library("topicmodels")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/postwar_era")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/neoliberal_era")
setwd("C:/Users/Scott.Malec/Desktop/SOTUS/neoliberal_era/neo_D")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/sixties/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/new_deal/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/jacksonian_democracy/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/early_republic/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/progressive_era/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/reconstruction/")
setwd("/home/hinckley/Public/corpora/SOTUS/")
setwd("/home/hinckley/Public/corpora/SOTUS/neoliberal_era/")
#setwd("/home/hinckley/Public/corpora/SOTUS/plutocrats/")
#setwd("/home/hinckley/Public/corpora/SOTUS/D_BHO/")
#setwd("/home/hinckley/Public/corpora/SOTUS/D_WJC/")
#setwd("/home/hinckley/Public/corpora/SOTUS/R_GHB/")
#setwd("/home/hinckley/Public/corpora/SOTUS/R_GWB/")
#setwd("/home/hinckley/Public/corpora/SOTUS/R_TRR/")
#setwd("/home/hinckley/Public/corpora/SOTUS/D_LBJ/")
#setwd("/home/hinckley/Public/corpora/SOTUS/D_WWI/")
#setwd("/home/hinckley/Public/corpora/SOTUS/D_AAJ/")
#setwd("/home/hinckley/Public/corpora/SOTUS/R_RWR/")
setwd("/home/hinckley/Public/corpora/SOTUS/F_JMO/")
#setwd("/home/hinckley/Public/corpora/SOTUS/better_angels/")
#setwd("/home/hinckley/Public/corpora/SOTUS/neoliberal_era/neo_D/")
#setwd("/home/hinckley/Public/corpora/SOTUS/neoliberal_era/neo_R/")
#setwd("/home/hinckley/Public/corpora/SOTUS/newdeal_postwar/")
#setwd("/home/hinckley/Public/corpora/SOTUS/newdeal_postwar/")
#setwd("/home/hinckley/Public/corpora/SOTUS/progressive_newdeal/")
#setwd("/home/hinckley/Public/corpora/SOTUS/early_republic/")
#setwd("/home/hinckley/Public/corpora/SOTUS/reconstruction/")
#setwd("/home/hinckley/Public/corpora/SOTUS/roaring_twenties/")
#setwd("/home/hinckley/Public/corpora/SOTUS/plutocrats/")
setwd("/home/hinckley/Public/corpora/SOTUS/N_GEW/")
setwd("/home/hinckley/Public/corpora/SOTUS/F_JQA/")
#wd = homePath
#setwd(wd)
text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource('.'))
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "mr speaker", "nation", "nations", "fy","congress",  "30th", "1981", "american people", "past", "1982", "vice", "president", "state union", "year", "years", "united states", "my administration", "fellow citizens", "god bless", "must", "can", "lot people", "men", "meet", "work", "close"))
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "senate", "house", "representatives", "state", "union", "1st", "american", "annual message"))
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "distinguished", "members", "honored", "guests", "mr", "people", "made", "weve"))
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "say", "democrats", "next", "give", "go", "us", "republicans", "proposing", "theres", "im", "want", "will", "let", "id", "let"))
#corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
corpus <- tm_map(corpus, stripWhitespace)
pre_dtm <- DocumentTermMatrix(corpus)
myDictionary <- list(pre_dtm$dimnames)
#corpus <- tm_map(corpus, stemDocument)
#corpus <- tm_map(corpus, stemCompletion(dictionary=myDictionary))
#corpus <- tm_map(corpus, stemCompletionion(corpus, dictionary=, type=("prevalent")))
yourTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min=2, max=4))
dtm <- DocumentTermMatrix(corpus, control=list(weighting = weightTf, tokenize=yourTokenizer))
dtm <- removeSparseTerms(dtm, .95)
#tdm <- TermDocumentMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), tokenize=yourTokenizer, stopwords = TRUE))
#tdm <- removeSparseTerms(tdm, .95)
print("##### we now have a tdm")
#dtm_complete = hclust(dist(dtm), method="complete")
#dtm <- removeSparseTerms(dtm, .95)
#plot(hclust(dist(dtm), method="complete"), xlab="text from corpus", "ylab"="distance", main="Cluster Dendrogram of Various Texts")
#op = par(bg="#DDE3CA")
#plot(dtm_complete, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
# col.axis="#F38630", lwd=1, lty=1, sub='', hang=-1, axes=FALSE,
# main = "Cluster Dendrogram Representing Author Similarity",
# xlab="Author/Publication", ylab = "Stylistic Distance")
#par(op)
############################
#create topic model using VEM, Gibbs sampling, fixed VEM
print("setting topic # or K, and seed of random gen")
K <- 8
SEED <- 1441
print("LDA ifying the DTM")
print("performing VEM, Gibbs, VEM_fixed on DTM")
#This part can take a while, depending on how many documents you have
greenspan_TM <-
  list(VEM = LDA(dtm, k = K, control = list(seed = SEED)), #note that DTM is required to be weighted with weightTf, term frequency
       Gibbs = LDA(dtm, k = K,
                   control = list(seed = SEED)),
       VEM_fixed = LDA(dtm, k = K,
                       control = list(estimate.alpha = TRUE, seed = SEED)))
#print("performing Gibbs sampling")
#greenspan_TM <-
# list(Gibbs = LDA(dtm, k = K,
# control = list(seed = SEED)))
print("processing Topic Model")
sapply(greenspan_TM[1:2], slot, "alpha")
sapply(greenspan_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))
greenspan_TM$Gibbs
print("topics for corpus")
Topic <- topics(greenspan_TM[["Gibbs"]], 1)
print("terms for corpus")
text <- paste("espeak -p 99 \"", "terms from corpus", "\"", sep="")
system(text)
Terms <- terms(greenspan_TM[["Gibbs"]], 20) #I sometimes do 10, depending on size of corpus
#lda <- LDA(dtm, control = list(alpha = 0.1), K)
Terms[,1] #view terms to see how "clean" the topics are, adjust K, seed, and other params as needed to obtain a clean topic set
Terms[,2]
Terms
gammaDF <- as.data.frame(greenspan_TM$Gibbs@gamma) #key step for gathering topic proportions!
plot(gammaDF)
