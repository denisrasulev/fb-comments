# Facebook Comments Exploration and Analysis
# (c) 2017 Denis Rasulev
# All Rights Reserved

# set working directory
setwd('/Volumes/data/projects/fb_sentiment/')

# load required libraries and functions
library(tm)         # Framework for text mining applications within R
library(NLP)        # Basic classes and methods for Natural Language Processing
library(ggplot2)    # Implementation of the grammar of graphics in R
library(wordcloud2) # Fast visualization tool for creating wordcloud
source("parser.r")
source("helper.r")

# if parsed file does not exist
if (!file.exists("data/comments.rds")) {

     # then load pre-processed comments file
     comments_file  <- readLines("data/comments_processed.txt",
                                 encoding = "UTF-8", skipNul = FALSE, warn = FALSE)

     # parse everything from it
     parsed <- parse_comments(comments_file)

     # and save it to disk
     saveRDS(parsed, file = "data/comments.rds")

     # clear memory
     rm(comments_file, parse_comments, parsed)
}

# if parsed file already exists, read it in
df_comments <- readRDS("data/comments.rds")

# --- Basic Time Analysis ------------------------------------------------------

# aggregate data by time frame
t1 <- table(df_comments$year)   # year
t2 <- table(df_comments$month)  # month
t3 <- table(df_comments$day)    # day
t4 <- table(df_comments$hour)   # hour

# distribution of comments by year
par(mar = c(3,5,3,1) + 0.1)
barplot(t1,
        col = "lightgreen",
        ylim = c(0,12000),
        las = 1)
title("Number of Comments by Year", adj = 0.5, line = 1)

# distribution of comments by month
barplot(t2,
        col = "lightgreen",
        ylim = c(0,12000),
        las = 1)
title("Number of Comments by Month", adj = 0.5, line = 1)

# distribution of comments by day
barplot(t3,
        col = "lightgreen",
        ylim = c(0,5000),
        las = 1)
title("Number of Comments by Day", adj = 0.5, line = 1)

# distribution of comments by hour
barplot(t4,
        col = "lightgreen",
        ylim = c(0,800),
        las = 1)
title("Number of Comments by Hour", adj = 0.5, line = 1)

# --- Exploratory Analysis 1 - Top Comment/er ----------------------------------

# top commenters by number of comments
t <- as.data.frame(table(df_comments$name))
t <- t[order(t$Freq, decreasing = TRUE),]
names(t)[1] = 'name'
names(t)[2] = 'comments'

# show top commenters as bar plot
par(mar = c(3,12,3,1) + 0.1)
barplot(t$comments[1:30],
        names.arg = t$name[1:30],
        col = rainbow(45),
        xlim = c(0,300),
        ylim = c(35,0),
        horiz = TRUE,
        las = 1)
grid(NULL, NA, lwd = 1, col = "lightgray", lty = "dotted")
title("Top 30 people by the number of posted comments", adj = 0, line = 0.5)

# most liked commenters
v <- df_comments[order(df_comments$like, decreasing = TRUE),]

# show top most liked as bar plot
par(mar = c(3,12,3,1) + 0.1)
barplot(v$like[1:30],
        names.arg = v$name[1:30],
        col = "lightgreen",
        xlim = c(0,100),
        ylim = c(35,0),
        horiz = TRUE,
        las = 1)
grid(NULL, NA, lwd = 1, col = "lightgray", lty = "dotted")
title("Top 30 people whose comments got most likes", adj = 0, line = 0.5)

# clean memory
rm(v)

# most lengthy comment
comment_length = 0
for (i in 1:nrow(df_comments)) {
     if (nchar(df_comments$cmnt[i]) > comment_length) {
          comment_length <- nchar(df_comments$cmnt[i])
          index <- i
     }
}

# print out findings
sprintf("Author of the most lengthy comment is %s", df_comments$name[index])
number_of_words <- sapply(gregexpr("\\W+", df_comments$cmnt[index]), length) + 1
sprintf("The comment contains %d characters and %d words",
        nchar(df_comments$cmnt[index]), number_of_words)

# --- Exploratory Analysis 2 - Corpus & Word Cloud -----------------------------

# because we have relatively small number of documents we will use simple corpus
df_corpus = Corpus(VectorSource(df_comments$cmnt), readerControl = list(language = "rus"))

# load list of russian stop words
ru_stopwords <- readLines("res/stop_words_ru.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
kz_stopwords <- readLines("res/stop_words_kz.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)

# remove contact information in the beginning of the file
ru_stopwords <- ru_stopwords[5:length(ru_stopwords)]
kz_stopwords <- kz_stopwords[5:length(kz_stopwords)]

# combine extended and standard stopwords lists
extended_stopwords <- c(stopwords('russian'), ru_stopwords, kz_stopwords)

# pre-process corpus
df_corpus <- tm_map(df_corpus, removeNumbers)
df_corpus <- tm_map(df_corpus, removePunctuation)
df_corpus <- tm_map(df_corpus, content_transformer(tolower))

# replace often misspelled significant word for correct spelling with diacritics
df_corpus <- tm_map(df_corpus, content_transformer(gsub),
                    pattern = "уят", replacement = "ұят")

# remove insignificant words
words_to_remove <- c("like","wink","smile","photo","emoticon")
df_corpus <- tm_map(df_corpus, removeWords, words_to_remove)

# remove stop words and extra spaces
df_corpus <- tm_map(df_corpus, removeWords, extended_stopwords)
df_corpus <- tm_map(df_corpus, stripWhitespace)

# create term-document matrix
tdm <- TermDocumentMatrix(df_corpus)

# remove sparse words:
# 0.99999 - remain all words, nothing is deleted
# 0.9999  - remain words encountered more than 2 times
# 0.999   - remain words encountered more than 10 times
tdm <- removeSparseTerms(tdm, 0.999)

# create data frame with words sorted by frequency
d <- sort_freq(tdm)

# show top words as bar plot
par(mar = c(3,6,3,1) + 0.1)
barplot(d[1:30,]$freq,
        names.arg = d$word[1:30],
        col = "lightgreen",
        xlim = c(0,600),
        ylim = c(35,0),
        horiz = TRUE,
        las = 1)
grid(NULL, NA, lwd = 1, col = "lightgray", lty = "dotted")
title("Top 30 most frequently used words", adj = 0, line = 0.5)

# build word cloud
set.seed(2017)
wordcloud2(data = d)

# --- Sentiment Analysis -------------------------------------------------------

# this feature is yet to be ralized some time later due to lack of resources for
# russian language sentiment analysis

# eof
