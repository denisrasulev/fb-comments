# Facebook comments Parser
# (c) 2017 Denis Rasulev
# All Rights Reserved.

parse_comments <- function(comments) {
     # this function goes through comments file line by line, finds and saves
     # to data frame - name, text of comment, number of likes and date posted
     # returns data.frame['name','cmnt','like','date']

     # save length of comments file
     number_of_rows <- length(comments)

     # prepare empty data frame to store name, comment, likes and date
     df <- data.frame(matrix(ncol = 4, nrow = number_of_rows))
     colnames(df) <- c('name','cmnt','like','date')

     for (i in 1:number_of_rows ) {
          if ( comments[i] == "" ) {

               # first string after empty one is commenter's name
               df[i, 'name'] <- comments[i + 1]

               # second string after empty one is text of a comment
               # it starts with name of a commenter so we remove it
               comment_text <- sub(paste0(comments[i + 1],' '), '', comments[i + 2])

               # it may consist of several line so we need index to read many lines
               j <- 3

               # while next line doesn't start with middle dot '·' (unicode 00B7)
               while (substring(comments[i + j], 1, 1) != '\U00B7') {

                    # in case we reach end of file we need to break the loop
                    if ( i + j > number_of_rows ) {
                         break
                    }

                    # add every line to comment
                    comment_text <- paste(comment_text, comments[i + j])
                    j <- j + 1
               }

               # save text of a comment
               df[i, 'cmnt'] <- comment_text

               # save number of likes for a comment, removing midle dot
               df[i, 'like'] <- sub('\U00B7 ', '', comments[i + j])

               # save a date when comment was posted, removing midle dot
               df[i, 'date'] <- sub('\U00B7 ', '', comments[i + j + 1])
          }
     }

     # remove empty rows, consisting only of NAs
     df <- na.omit(df)

     # convert number of likes from character to number
     df[,'like'] <- as.numeric(df[,'like'])

     # return clean data frame
     return(df)
}
