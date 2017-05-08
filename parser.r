# comments parser

parse_comments <- function(df, txt) {
     # this function goes through comments file line by line, finds and saves
     # name, text of comment, number of likes and date posted to data frame.

     for (i in 1:length(txt) ) {
          if ( txt[i] == "" ) {

               # first string after empty one is commenter's name; save it
               df[i, "name"] <- txt[i + 1]

               # second string after empty one is text of a comment.
               long_comment <- sub(paste0(txt[i + 1],' '), '', txt[i + 2]) # removing name from the text of comment

               # it may consist of several lines, so while next line doesn't start with middle dot '·' (unicode 00B7) continue to add text
               j <- 3
               while (!grepl('\U00B7', txt[i + j])) {
                    # in case we reach end of file we need to break while loop
                    if (is.na(long_comment)) {
                         break
                    }
                    # add next line to our long, multi-line comment
                    long_comment <- paste(long_comment, txt[i + j])
                    j <- j + 1
               }

               # save text of a comment
               df[i, "cmnt"] <- long_comment

               # save number of likes for a comment, removing midle dote
               df[i, "like"] <- sub('\U00B7 ', '', txt[i + j])

               # save date a comment was posted, removing midle dote
               df[i, "date"] <- sub('\U00B7 ', '', txt[i + j + 1])
          }
     }

     # remove empty rows, consisting only of NAs
     df <- na.omit(df)

     # convert number of likes from character to number
     df[,'like'] <- as.numeric(df[,'like'])

     # return clean data frame
     return(df)
}
