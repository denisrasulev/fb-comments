library(ggplot2)
library(lubridate)
source("/Volumes/data/projects/fb_sentiment/parser.r")

# read comments file
setwd('/Volumes/data/projects/fb_sentiment/')
comments  <- readLines("comments_processed.txt", encoding = "UTF-8", ok = TRUE, skipNul = FALSE, warn = FALSE)

# prepare empty data frame to store name, comment, likes and date
comments_table <- data.frame(
     name = character(0),
     cmnt = character(0),
     like = numeric(0),
     date = character(0),
     stringsAsFactors = FALSE
)

# parse comments
clean <- parse_comments(comments_table, comments)

# split date column for further analysis
clean[,'dt']    <- parse_date_time(clean[,'date'], orders = "mdy IMp")
clean[,'year']  <- year(clean[,'dt'])
clean[,'month'] <- month(clean[,'dt'])
clean[,'day']   <- day(clean[,'dt'])
clean[,'hour']  <- hour(clean[,'dt'])

# remove unused columns
clean[,c('date','dt')] <- NULL

# Exploratory Analysis

# top commenter by number of comments
w = table(clean$name)
t = as.data.frame(w)
t <- t[order(t$Freq, decreasing = TRUE),]
names(t)[1] = 'Name'
names(t)[2] = 'Comments'

par(mai = c(.6,2,.1,.5))
barplot(t$Comments[1:30],
        col = rainbow(45),
        xlim = c(0,300),
        ylim = c(35,0),
        names.arg = t$Name[1:30],
        #cex.names = 0.8,
        horiz = TRUE,
        las = 1)

# top commenter by number of likes
v <- clean[order(clean$like, decreasing = TRUE),]

par(mai = c(.6,2.5,.1,.5))
barplot(v$like[1:30],
        col = rainbow(45),
        xlim = c(0,100),
        ylim = c(35,0),
        names.arg = v$name[1:30],
        #cex.names = 0.9,
        horiz = TRUE,
        las = 1)

# most lengthy comment - max(nchar(clean$cmnt))
comment_length = 0
for (i in 1:nrow(clean)) {
     if (nchar(clean$cmnt[i]) > comment_length) {
          comment_length <- nchar(clean$cmnt[i])
          index <- i
     }
}
sprintf("Author of most lengthy comment is %s", clean$name[index])
sprintf("Comments contains %d characters", nchar(clean$cmnt[index]))
print(clean$cmnt[index])
