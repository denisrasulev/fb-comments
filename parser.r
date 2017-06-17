# Parser for Comments from pre-Processed File
# (c) 2017 Denis Rasulev
# All Rights Reserved.

parse_comments <- function(comments) {
     # this function goes through pre-processed comments file row by row,
     # finds information by certain markers and saves it to data frame as
     # name, text of comment, number of likes and date posted
     # then returns data.frame['name','cmnt','like','date']

     # save length of file with comments
     number_of_rows <- length(comments)

     # prepare empty data frame to store name, comment, likes and date
     df <- data.frame(matrix(ncol = 4, nrow = number_of_rows))
     colnames(df) <- c('name','cmnt','like','date')

     for (i in 1:number_of_rows ) {

          # if row is empty...
          if ( comments[i] == "" ) {

               # then next row after empty one contains commenter's name
               df[i, 'name'] <- comments[i + 1]

               # third row after empty one contains text of a comment and
               # it always starts with the name of a commenter so we remove it
               comment_text <- sub(paste0(comments[i + 1],' '), '', comments[i + 2])

               # text of a comment may be on several lines so we need index
               # to read them all
               j <- 3

               # while next line doesn't start with middle dot '·' (unicode 00B7)
               while (substring(comments[i + j], 1, 1) != '\U00B7') {

                    # if we reach end of file we need to break the loop
                    if ( i + j > number_of_rows ) {
                         break
                    }

                    # add every line to comment
                    comment_text <- paste(comment_text, comments[i + j])
                    j <- j + 1
               }

               # save complete text of a comment
               df[i, 'cmnt'] <- comment_text

               # save number of likes for a comment, removing midle dot
               df[i, 'like'] <- sub('\U00B7 ', '', comments[i + j])

               # save date when a comment was posted, removing midle dot
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
