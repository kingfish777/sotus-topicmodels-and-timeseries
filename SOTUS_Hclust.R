#############
library(tm)
library(ape)
library(RWeka)
home <- "/home/hinckley"
homePath = paste(home, "/Public/corpora/SOTUS", sep="")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/sixties/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/new_deal/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/jacksonian_democracy/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/early_republic/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/progressive_era/")
#setwd("C:/Users/Scott.Malec/Desktop/SOTUS/reconstruction/")
#setwd("/home/hinckley/Public/corpora/SOTUS/")
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
#setwd("/home/hinckley/Public/corpora/SOTUS/F_JMO/")
#setwd("/home/hinckley/Public/corpora/SOTUS/better_angels/")
#setwd("/home/hinckley/Public/corpora/SOTUS/neoliberal_era/neo_D/")
#setwd("/home/hinckley/Public/corpora/SOTUS/neoliberal_era/neo_R/")
#setwd("/home/hinckley/Public/corpora/SOTUS/newdeal_postwar/")
#setwd("/home/hinckley/Public/corpora/SOTUS/newdeal_postwar/")
#setwd("/home/hinckley/Public/corpora/SOTUS/progressive_newdeal/")
#setwd("/home/hinckley/Public/corpora/SOTUS/early_republic/")
#setwd("/home/hinckley/Public/corpora/SOTUS/reconstruction/")
#setwd("/home/hinckley/Public/corpora/SOTUS/roaring_twenties/")
##setwd("/home/hinckley/Public/corpora/SOTUS/plutocrats/")
setwd(paste(homePath, sep=""))
setwd("/home/hinckley/Public/corpora/SOTUS/Democrats/")
setwd("/home/hinckley/Public/corpora/SOTUS/Republicans/")
text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource())
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language = "english")
ngrams <- RWeka::NGramTokenizer(corpus, Weka_control(min=1, max=4))
dtm <- DocumentTermMatrix(corpus, control = list(ngrams, wordLengths=c(3, 25), weighting = weightTfIdf, stopwords=TRUE))
rowTotals <- apply(dtm, 1, sum) #Find the sum of words in each Document
dtm <- dtm[rowTotals> 0 || rowTotals==NA] #remove all docs without words
dtm <- removeSparseTerms(dtm, .95)
dtm_complete = hclust(dist(dtm), method="complete")
plot(hclust(dist(dtm), method="complete"), xlab="text from corpus", "ylab"="distance", main="Cluster Dendrogram of Various Texts")
op = par(bg="#DDE3CA")
plot(dtm_complete, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=1, lty=1, sub='', hang=-1, axes=FALSE,
     main = "Cluster Dendrogram Representing Author Similarity",
     xlab="Author/Publication", ylab = "Stylistic Distance")
#par(op)
#plot(dtm_complete, hang=1, axes = TRUE, ann=TRUE, main = "Cluster Dendrogram Representing Author Similarity",
#      xlab="Author/Publication", ylab = "Distance")
#phyl <- as.phylo(dtm_complete)
#plot(phyl, edge.col=c("blue", "green", "red")[c(TRUE, FALSE) + 1 + (phyl$edge.length > 20)])
############
