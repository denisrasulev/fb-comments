# Facebook post analytics
# © Denis Rasulev 2016

# load necessary R librabires
library(tm)         # Framework for text mining applications within R
library(NLP)        # Basic classes and methods for Natural Language Processing
library(slam)       # Data structures and algorithms for sparse arrays and matrices
library(ggplot2)    # Implementation of the grammar of graphics in R

# FUNCTIONS DECLARATIONS BLOCK

# function for cleaning
clean <- function(x)
{
    # convert everything to lower case
    x <- tolower(x)

    # remove numbers and control symbols
    x <- gsub("[[:digit:]]", "", x)
    x <- gsub("[[:cntrl:]]", "", x)

    # remove web addresses and urls
    x <- gsub(" www(.+) ", "", x)
    x <- gsub(" http(.+) ", "", x)

    # remove punctuations marks
    x <- gsub("[[:punct:]]", "", x)

    # remove remaining single letters (repeat 5 times)
    x <- gsub("\\b [а-яё]\\b", "", x)
    x <- gsub("\\b [а-яё]\\b", "", x)
    x <- gsub("\\b [а-яё]\\b", "", x)
    x <- gsub("\\b [а-яё]\\b", "", x)
    x <- gsub("\\b [а-яё]\\b", "", x)

    # remove single letters in the beginning of sentence
    x <- gsub("\\b[а-яё]\\b ", "", x)

    # remove leading and trailing spaces
    x <- gsub("^\\s+|\\s+$", "", x)

    return(x)
}

# function for sorting n-grams in decreasing order
sort.freq <- function(x){
    srt <- sort(row_sums(x, na.rm = T), decreasing = TRUE)
    frf <- data.frame(ngram = names(srt), freq = srt, row.names = NULL, check.rows = TRUE,  stringsAsFactors = FALSE)
    return(frf)
}

# functions for tokenization
n1gram <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
n2gram <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
n3gram <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

# MAIN PROCESSING BLOCK

# read data
coments  <- readLines("comments_semi_clean_2.txt", encoding = "UTF-8", ok = TRUE, skipNul = TRUE, warn = FALSE)
cln.cmts <- clean(coments)
write(cln.cmts, file = "auto_cleaned.txt")

# load list of russian stop words
stopwords <- readLines("russian-stop-words.txt", encoding = "UTF-8", ok = TRUE, skipNul = TRUE, warn = FALSE)

# create our corpus and clean it
corp <- VCorpus(VectorSource(cln.cmts))
corp <- tm_map(corp, stripWhitespace)
copr <- tm_map(corp, removeWords, stopwords)

# create term document matrix for unigrams, reduce sparsity and save
tdm1 <- TermDocumentMatrix(corp, control = list(tokenize = n1gram))
tdm1 <- removeSparseTerms(tdm1, 0.99999)
frq1 <- sort.freq(tdm1)
# saveRDS(frq1, file = "~/Volumes/data/coursera/capstone/capapp/CapApp/data/data1.RDS")

ggplot(frq1[1:30,],
        aes(x = reorder(ngram, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "green", col = "black") +
        theme_bw() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Frequency", title = "Most common Unigrams in the Sample")

# create term document matrix for bigrams, reduce sparsity and save
tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = n2gram))
tdm2 <- removeSparseTerms(tdm2, 0.99999)
frq2 <- sort.freq(tdm2)
# saveRDS(frq2, file = "/Volumes/data/coursera/capstone/capapp/CapApp/data/data2.RDS")

ggplot(frq2[1:30,],
        aes(x = reorder(ngram, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "green", col = "black") +
        theme_bw() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Frequency", title = "Most common Bigrams in the Sample")

# create term document matrix for trigrams, reduce sparsity and save
tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = n3gram))
tdm3 <- removeSparseTerms(tdm3, 0.99999)
frq3 <- sort.freq(tdm3)
# saveRDS(frq3, file = "/Volumes/data/coursera/capstone/capapp/CapApp/data/data3.RDS")

ggplot(frq3[1:30,],
        aes(x = reorder(ngram, freq), y = freq)) +
        geom_bar(stat = "identity", fill = "green", col = "black") +
        theme_bw() +
        coord_flip() +
        theme(axis.title.y = element_blank()) +
        labs(y = "Frequency", title = "Most common Trigrams in the Sample")

# eof
