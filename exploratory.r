# Facebook Comments Exploration
# (c) 2017 Denis Rasulev
# All Rights Reserved.

# load required libraries and functions
library(tm)
suppressWarnings(library(ggplot2))
suppressWarnings(library(lubridate))
source("/Volumes/data/projects/fb_sentiment/parser.r")

# read comments file
setwd('/Volumes/data/projects/fb_sentiment/')
comments_file  <- readLines("comments_processed.txt", encoding = "UTF-8",
                            ok = TRUE, skipNul = FALSE, warn = FALSE)

# parse comments
clean <- parse_comments(comments_file)

# split date column for further analysis
clean[,'dt']    <- parse_date_time(clean[,'date'], orders = "mdy IMp")
clean[,'year']  <- year(clean[,'dt'])
clean[,'month'] <- month(clean[,'dt'])
clean[,'day']   <- day(clean[,'dt'])
clean[,'hour']  <- hour(clean[,'dt'])

# remove unused columns
clean[,c('date','dt')] <- NULL

# Exploratory Analysis 1
# ==============================================================================

# top commenter by number of comments
w <- table(clean$name)
t <- as.data.frame(w)
t <- t[order(t$Freq, decreasing = TRUE),]
names(t)[1] = 'Name'
names(t)[2] = 'Comments'

par(mar = c(3,10,2,1), adj = 0)
barplot(t$Comments[1:30],
        main = "Top 30 commenters",
        names.arg = t$Name[1:30],
        col = rainbow(45),
        xlim = c(0,300),
        ylim = c(35,0),
        horiz = TRUE,
        line = 0,
        las = 1)

# most liked comment/commenter
v <- clean[order(clean$like, decreasing = TRUE),]

#par(mar = c(3,10,2,1), adj = 0)
barplot(v$like[1:30],
        main = "Top 30 most liked",
        names.arg = v$name[1:30],
        col = rainbow(45),
        xlim = c(0,100),
        ylim = c(35,0),
        horiz = TRUE,
        line = -0.1,
        las = 1)

# most lengthy comment - max(nchar(clean$cmnt))
comment_length = 0
for (i in 1:nrow(clean)) {
     if (nchar(clean$cmnt[i]) > comment_length) {
          comment_length <- nchar(clean$cmnt[i])
          index <- i
     }
}
sprintf("Author of the most lengthy comment is %s", clean$name[index])
number_of_words <- sapply(gregexpr("\\W+", clean$cmnt[index]), length) + 1
sprintf("The comment contains %d characters and %d words",
        nchar(clean$cmnt[index]), number_of_words)

# Exploratory Analysis 2
# ==============================================================================

dfCorpus = Corpus(VectorSource(clean$cmnt))

dfCorpus <- tm_map(dfCorpus, tolower)
dfCorpus <- tm_map(dfCorpus, removeNumbers)
dfCorpus <- tm_map(dfCorpus, removePunctuation)
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords('russian'))
dfCorpus <- tm_map(dfCorpus, removeWords, c("вы", "её", "не", "это", "жок",
                                            "деп", "фото", "екен", "photo",
                                            "emoticon"))
dfCorpus <- tm_map(dfCorpus, stripWhitespace)
#dfCorpus <- tm_map(dfCorpus, stemDocument)
#dfCorpus <- tm_map(dfCorpus, PlainTextDocument)

tdm <- TermDocumentMatrix(dfCorpus)

v <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
#d <- d[order(-d[,2]),]
head(d, 10)

set.seed(12345)

# library(wordcloud)
# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words = 200, random.order = FALSE, rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

library(wordcloud2)
wordcloud2(data = d)

findFreqTerms(tdm, lowfreq = 100)

par(mar = c(3,6,2,1), adj = 0)
barplot(d[1:30,]$freq,
        main = "Top 30 words",
        names.arg = d$word[1:30],
        #col = c("lightblue","red","green"),
        col = "lightgreen",
        #col = rainbow(90),
        xlim = c(0,450),
        ylim = c(35,0),
        horiz = TRUE,
        line = 0,
        las = 1)
