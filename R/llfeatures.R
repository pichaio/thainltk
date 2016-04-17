
#' Convert string to unigram and bigram feature id
#'
#' Intended for internal use.
#' convert string to unigrams and bigrams of characters. Then map to feature Id.
#' At any text position, 4 features are created. Those are unigram and bigram at the
#' position (the bigram starts at the previous position) and those after the position.
#'
#' @param s string to be converted
#' @param ngrams list of unigrams and bigrams. The position of each one will be used to calculate the feature ids.
#' @param baseFeatureIdx base feature id, if not starting from 0
#' @param n The number of the features
#'
#' @return list containing integer vectors of feature ids
#'
up2bigramFeatures <- function(s, ngrams, baseFeatureIdx = 0, n = length(ngrams)){
  unigrams <- strsplit(s,'')[[1]]
  bigrams <- paste0(c(' ', unigrams, ' '), c(unigrams, ' ', ' '))
  unname(Map(function (a, b, c, d){
    y <- c(a,b,c,d)
    y[!is.na(y)]
    },
    match(unigrams, ngrams) + baseFeatureIdx,
    match(bigrams[1:length(unigrams)], ngrams) + baseFeatureIdx,
    match(c(unigrams[-1], ' '), ngrams) + baseFeatureIdx + n,
    match(bigrams[-c(1, 2)], ngrams) + baseFeatureIdx + n)
    )
}

# create labels [1,2] for each position in the document. The document
# must be annotated with 'word'
createLabels <- function(doc){
  a1 <- NLP::annotations(doc)[[1]]
  a1 <- Filter(function (a) a$type == 'word', a1)
  y <- vapply(a1, function(x) as.integer(c(x$start - 1, x$end)), integer(2), USE.NAMES = F)
  y <- unique(as.vector(y))
  y <- y[y > 0]
  a <- rep(1L, nchar(doc$content))
  a[y] <- 2L
  a
}

getNumFeaturesV2_1 <- function(){
  length(knownWords) * 3  + 9 + length(charBigrams) * 2 + 7 * length(defaultLevels[[1]]) + 7 * length(defaultLevels[[2]])
}

getNumFeatures <- function(){
  getNumFeaturesV2_1()
}

#' Convert feature ids to matrix
#'
#' convert list of features for each text position to matrix p by n
#' p is the feature number and n is the training observation number.
#' the output is a logical sparse matrix.
#'
#' @param data list of features for each text position
#' @param numFeatures total number of features
#'
#' @return a logical sparse matrix.  p is the feature number and n is the training observation number.
#' @import Matrix
#'
features2Matrix <- function(data, numFeatures = getNumFeatures()){
  X <- as(Matrix::Matrix(0L, ncol = length(data), nrow = numFeatures, sparse = T), 'nsparseMatrix')
  colIds <- rep(seq_along(data), vapply(data, length, integer(1)))
  rowIds <- unlist(data)
  idx <- matrix(c(rowIds, colIds), ncol = 2)
  X[idx] <- T
  X
}

# map word to feature id.
# x is a list of character vectors. Each element of the list is for a position in text
# character vectors contains words at the position
mapspan <- function(x, baseIdx = 0L){
  ridx <- rep(seq_along(x), vapply(x, length, integer(1)))
  xu <- as.integer(match(unlist(x), knownWords) + baseIdx)
  gb <- split(xu, ridx)
  y <- list()
  length(y) <- length(x)
  y[as.integer(names(gb))] <- lapply(gb, c)
  y
}


# create word features around each position in the text.
# return a list of integer vectors, each containing feature id of that text position.
createWordspanFeatures <- function(s){
  maxChar <- max(nchar(knownWords))
  bspan <- allPossibleSpans(s, wordTrie, maxChar, knownWords)
  n <- length(knownWords)
  espan <- lapply(seq_along(bspan), function (i) character(0))
  btspan <- lapply(seq_along(bspan), function (i) character(0))

  invisible(Map(function(wl, i){
    lapply(wl, function(w){
      nc <- nchar(w)
      espan[[i + nc]] <<- c(espan[[i + nc]], w)
      if (nc > 2){
        lapply(seq_len(nc - 2) + 1, function(wi){
          btspan[[i + wi]] <<- c(btspan[[i + wi]], w)
        })
      }
    })
    NULL
  }, bspan, seq_along(bspan) - 1))
  # bspan of next char
  bspan <- bspan[-1]
  bspan[[length(bspan) + 1]] <- character(0)
  hasb <- vapply(bspan, function (x) length(x) > 0, logical(1))
  hase <- vapply(espan, function (x) length(x) > 0, logical(1))
  hasbt <- vapply(btspan, function (x) length(x) > 0, logical(1))
  hasbonly <- hasb & !(hase & hasbt)
  haseonly <- hase & !(hasb & hasbt)
  hasbtonly <- hasbt & !(hasb & hase)
  hasbande <- hasb & hase
  hasbandeonly <- hasb & hase & !hasbt
  hasall <- hasb & hase & hasbt
  hasb[hasb] <- as.integer(3 * n + 1)
  hase[hase] <- as.integer(3 * n + 2)
  hasbt[hasbt] <- as.integer(3 * n + 3)
  hasbonly[hasbonly] <- as.integer(3 * n + 4)
  haseonly[haseonly] <- as.integer(3 * n + 5)
  hasbtonly[hasbtonly] <- as.integer(3 * n + 6)
  hasbande[hasbande] <- as.integer(3 * n + 7)
  hasbandeonly[hasbandeonly] <- as.integer(3 * n + 8)
  hasall[hasall] <- as.integer(3 * n + 9)
  hasb[hasb == 0] <- NA
  hase[hase == 0] <- NA
  hasbt[hasbt == 0] <- NA
  hasbonly[hasbonly == 0] <- NA
  haseonly[haseonly == 0] <- NA
  hasbtonly[hasbtonly == 0] <- NA
  hasbande[hasbande == 0] <- NA
  hasbandeonly[hasbandeonly == 0] <- NA
  hasall[hasall == 0] <- NA
  # 25.8 (6.3)
  Map(function (...){
    y <- c(...)
    y <- y[!is.na(y)]
    y
  }, mapspan(bspan), mapspan(espan, n), mapspan(btspan, 2*n),
  hasb, hase, hasbt, hasbonly, haseonly, hasbtonly, hasbande, hasbandeonly, hasall)
}


# create a list of integer vectors, each containing feature ids for that particular text position
createFeaturesV2 <- function(s){
  n <- length(knownWords)
  d1 <- createWordspanFeatures(s)
  s <- gsub('[[:space:]]', ' ', s)
  d <- "[\u0e51\u0e52\u0e53\u0e54\u0e55\u0e56\u0e57\u0e58\u0e59\u0e501234567890]"
  s <- gsub('[[:alpha:]]', 'a', s)
  s <- gsub(d , 'd', s)
  d2 <- up2bigramFeatures(s, charBigrams, baseFeatureIdx = n * 3 + 9)
  baseIdx1 <- seq(n * 3 + length(charBigrams) * 2 + 9, by = length(defaultLevels[[1]]), length.out = 7)
  baseIdx2 <- seq(baseIdx1[[length(baseIdx1)]] + length(defaultLevels[[1]]),
                  by = length(defaultLevels[[2]]), length.out = 7)
  x <- char2modelInput(as.character(s))
  x[] <- lapply(x, as.integer)
  x <- as.matrix(x)
  x <- sweep(x, 2, c(baseIdx1, baseIdx2), `+`)
  lapply(seq_along(d1), function(i) c(d1[[i]], d2[[i]], unname(x[i, ])))
}
