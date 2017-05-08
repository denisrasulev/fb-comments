library(lubridate)

setwd('/Volumes/data/projects/fb_sentiment/')
comments  <- readLines("comments_processed.txt", encoding = "UTF-8", ok = TRUE, skipNul = FALSE, warn = FALSE)

parse_comments <- function(df, txt) {

     for (i in 1:length(txt) ) {
          if ( txt[i] == "" ) {

               df[i, "name"] <- txt[i + 1]

               long_comment <- sub(paste0(txt[i + 1],' '), '', txt[i + 2]) # removing name from the text of comment

               j <- 3
               while (!grepl('\U00B7', txt[i + j])) { # while next line doesn't start with middle dot '·' continue to add text
                    if (is.na(long_comment)) break
                    long_comment <- paste(long_comment, txt[i + j])
                    j <- j + 1
               }

               df[i, "cmnt"] <- long_comment

               df[i, "like"] <- sub('\U00B7 ', '', txt[i + j])

               df[i, "date"] <- sub('\U00B7 ', '', txt[i + j + 1])
          }
     }

     return(df)
}
