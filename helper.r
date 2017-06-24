# Helper file - set of functions to be used
# (c) 2017 Denis Rasulev
# All Rights Reserved

sort_freq <- function(x) {
     # this function receives Term-Document Matrix, sorts terms, removes NAs
     # and returns data frame with 'word' and 'freq' columns sorted by 'freq'
     # in decreasing order

     # load required libraries
     library(slam)

     # sort terms by their frequency in decreasing order while removing NAs
     srt <- sort(row_sums(x, na.rm = T), decreasing = TRUE)

     # save as data frame without row names and columns 'word', 'freq'
     frf <- data.frame(word = names(srt), freq = srt, row.names = NULL,
                       check.rows = TRUE,  stringsAsFactors = FALSE)

     # return sorted data frame
     return(frf)
}
