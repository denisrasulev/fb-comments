# Facebook Comments Exploration
# (c) 2017 Denis Rasulev
# All Rights Reserved.

# load required libraries and functions
library(tm)         # Framework for text mining applications within R
library(NLP)        # Basic classes and methods for Natural Language Processing
library(slam)       # Data structures and algorithms for sparse arrays & matrices
library(ggplot2)    # Implementation of the grammar of graphics in R
library(lubridate)  # Make Dealing with Dates a Little Easier
library(wordcloud2) # Fast visualization tool for creating wordcloud
source("/Volumes/data/projects/fb_sentiment/parser.r")

# read comments file
setwd('/Volumes/data/projects/fb_sentiment/')
comments_file  <- readLines("comments_processed.txt", encoding = "UTF-8",
                            ok = TRUE, skipNul = FALSE, warn = FALSE)

# parse comments from prepared file
df_comments <- parse_comments(comments_file)

# split date column for further analysis
df_comments[,'dt']    <- parse_date_time(df_comments[,'date'], orders = "mdy IMp")
df_comments[,'year']  <- year(df_comments[,'dt'])
df_comments[,'month'] <- month(df_comments[,'dt'])
df_comments[,'day']   <- day(df_comments[,'dt'])
df_comments[,'hour']  <- hour(df_comments[,'dt'])

# remove unused columns
df_comments[,c('date','dt')] <- NULL

# Exploratory Analysis 1 - Top something
# ==============================================================================

# who is top commenter by number of comments
w <- table(df_comments$name)
t <- as.data.frame(w)
t <- t[order(t$Freq, decreasing = TRUE),]
names(t)[1] = 'Name'
names(t)[2] = 'Comments'

# show top commenters as bar plot
par(mar = c(3,12,2,1))
barplot(t$Comments[1:30],
        names.arg = t$Name[1:30],
        col = rainbow(45),
        xlim = c(0,300),
        ylim = c(35,0),
        horiz = TRUE,
        las = 1)
title("Top 30 commenters", adj = 0, line = 0)

# most liked comment/commenter
v <- df_comments[order(df_comments$like, decreasing = TRUE),]

# show top most liked as bar plot
par(mar = c(3,12,2,1), adj = 0)
barplot(v$like[1:30],
        names.arg = v$name[1:30],
        col = "lightgreen",
        xlim = c(0,100),
        ylim = c(35,0),
        horiz = TRUE,
        las = 1)
title("Top 30 most liked commenters", adj = 0, line = 0)

# most lengthy comment - max(nchar(df_comments$cmnt))
comment_length = 0
for (i in 1:nrow(df_comments)) {
     if (nchar(df_comments$cmnt[i]) > comment_length) {
          comment_length <- nchar(df_comments$cmnt[i])
          index <- i
     }
}
sprintf("Author of the most lengthy comment is %s", df_comments$name[index])
number_of_words <- sapply(gregexpr("\\W+", df_comments$cmnt[index]), length) + 1
sprintf("The comment contains %d characters and %d words",
        nchar(df_comments$cmnt[index]), number_of_words)

# Exploratory Analysis 2 - Word Cloud
# ==============================================================================

# because we have relatively small number of documents we will use simple corpus
df_corpus = Corpus(VectorSource(df_comments$cmnt), readerControl = list(language = "rus"))

# pre-process corpus
df_corpus <- tm_map(df_corpus, content_transformer(tolower))
df_corpus <- tm_map(df_corpus, removeNumbers)
df_corpus <- tm_map(df_corpus, removePunctuation)
df_corpus <- tm_map(df_corpus, removeWords, stopwords('russian'))
df_corpus <- tm_map(df_corpus, removeWords, c("вы", "её", "не", "это", "жок",
                                            "деп", "фото", "екен", "photo",
                                            "emoticon"))
df_corpus <- tm_map(df_corpus, stripWhitespace)

# function for sorting words in decreasing order
sort_freq <- function(x){
     srt <- sort(row_sums(x, na.rm = T), decreasing = TRUE)
     frf <- data.frame(word = names(srt), freq = srt, row.names = NULL,
                       check.rows = TRUE,  stringsAsFactors = FALSE)
     return(frf)
}

# create term-document matrix
tdm <- TermDocumentMatrix(df_corpus)

# remove sparse words:
# 0.99999 - all in
# 0.9999  - words encountered more than 2 times
# 0.999   - words encountered more than 10 times
tdm <- removeSparseTerms(tdm, 0.999)

# create data frame with words sorted by frequency
d <- sort_freq(tdm)

# show top words as bar plot
par(mar = c(3,6,2,1))
barplot(d[1:30,]$freq,
        names.arg = d$word[1:30],
        col = "lightgreen",
        xlim = c(0,450),
        ylim = c(35,0),
        horiz = TRUE,
        las = 1)
title("Top 30 words", adj = 0, line = 0)

# build word cloud
set.seed(2017)
wordcloud2(data = d)

# Time Series Analysis
# ==============================================================================



# Sentiment Analysis
# ==============================================================================

