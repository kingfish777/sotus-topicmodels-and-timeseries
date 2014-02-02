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
# Date: 1/23/2014
#
# TITLE: scraper.R
#
# Purpose: R tools to scrape Federal Reserve text collection
#
#########################################################################



#####################################################
#ecclesCorpus
#####################################################
#scrape gReenspan database
# http://fraser.stlouisfed.org/docs/historic/eccles/(e|E)ccles_ddddmmdd.pdf
#####################################################
library(XML) #htmlTreeParse
library(tm)
library(openNLP)
library(RCurl)
library(RWeka)
setwd("/home/hinckley/POTUS/")
homePath = "/home/hinckley/POTUS"
startDoc = 5000
endDoc = 104000
duration = endDoc - startDoc
baseURL <- list("http://www.presidency.ucsb.edu/ws/index.php?pid=")
unlist(baseURL[1])
trim <- function(x) { gsub("\\s", "", x) }
duration = endDoc - startDoc
# http://www.presidency.ucsb.edu/ws/index.php?pid=29431
for (y in 1:duration) {
          docNum = startDoc + y
          url = paste(unlist(baseURL), unlist(docNum), sep="")
          print(url)
          if (url.exists(url)) {
            #
            dest = as.character(paste("ucsb_", unlist(docNum), ".txt", sep=""))
            url = as.character(url)
            doc.html <- htmlTreeParse(url, useInternal = TRUE)
            doc.txt <- unlist(xpathApply(doc.html, '//p', xmlValue))
            doc.txt <- gsub('\\n', ' ', doc.txt)
            doc.txt <- paste(doc.txt, collapse = ' ')
            print(doc.txt)
            write(doc.txt, file <- dest)
            print("found one!")
            #uri=url
            #dat <- download.file(doc.txt, dest, mode = "w")
            #txt_file_name <- sub(".txt", ".txt", dest)
            try(msg<-system(paste("htmltotext ", dest, sep="")))
            if (msg > 'NULL') {
              print(msg) }
            if (msg == "stderror")
            {
              system(paste("rm ", dest, sep=""));
              print("WE GOT ONE!")
            }
          } else {
            print("bubble trouble in the Temple")
          }
}

