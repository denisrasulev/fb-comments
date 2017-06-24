# Helper file - set of functions to be used
# (c) 2017 Denis Rasulev
# All Rights Reserved

# function for sorting words in decreasing order
sort_freq <- function(x){
     srt <- sort(row_sums(x, na.rm = T), decreasing = TRUE)
     frf <- data.frame(word = names(srt), freq = srt, row.names = NULL,
                       check.rows = TRUE,  stringsAsFactors = FALSE)
     return(frf)
}
