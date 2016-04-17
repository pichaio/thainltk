# prepare character bigrams for trainning. Note that this script
# is used for the final model so it looks through all documents.
# for training-validating loops, should use only those found in train data set.
library(tm)
getUp2Bigrams <- function (cor){
  allt <- sapply(cor, function (x) x$content)
  allt <- paste(allt, collapse = ' ')
  allt <- gsub('[[:alpha:]]', 'a', allt)
  #allt <- gsub('[[:digit:]]', 'd', allt)
  d <- "[\u0e51\u0e52\u0e53\u0e54\u0e55\u0e56\u0e57\u0e58\u0e59\u0e501234567890]"
  allt <- gsub(d , 'd', allt)
  allc <- strsplit(allt, '')[[1]]
  unigram <- unique(allc)
  bigrams <- paste0(allc[-1], allc[-length(allc)])
  ubigrams <- unique(bigrams)
  list(uni = unigram, bi = ubigrams)
}
cor <- readRDS('data-raw/besttrain.RDS')
train <- getUp2Bigrams(cor)
cor <- readRDS('data-raw/bestval.RDS')
val <- getUp2Bigrams(cor)
cor <- readRDS('data-raw/besttest.RDS')
test <- getUp2Bigrams(cor)
str(test)
uuni <- unique(c(train$uni, val$uni, test$uni))
ubi <- unique(c(train$bi, val$bi, test$bi))
charBigrams <- c(uuni, ubi)
