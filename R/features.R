#' Character ngram (internal)
#'
#' Create character ngram data frame from input string.
#'
#' Ngram is a popular input model for NLP. For this word tokenization problem, n-gram
#' model would be n characters before and n characters after a particular character
#' that we want to predict a word end. For example, a 3-gram input model will have 7 columns.
#' Paddings are added to the actucal begining and ending of the text so the number of
#' rows will be equal to the number of characters in the text.
#'
#' Generally, predictive models can learn better if the number of factor levels is not
#' too large. For example, RandomForest can handle about 60 levels. A common scheme to handle
#' factors is to use one-hot encoding. A factor column with n levels will be expanded
#' to n-1 columns with binary values. The number of one-hot encoded features can be
#' large if we allow any character to take one level in a factor.
#'
#' One way to tackle the problem is to abstract characters to a smaller number of classes.
#' The parameter mapper would do just this. Users can provide a mapper function that map
#' text to text, but with a smaller number of domain characters.
#'
#' This is an S3 method and the default implementation is to convert the input to character,
#' using as.character method. So it will fail if the input cannot be converted to character.
#'
#' Note: Intended to be used internally.
#'
#' @param x input
#' @param mapper map character to character, possibly to reduce the number of unique values
#' @param n the number of gram
#'
#' @return data frame. Each column is of character type, not factor.
ngram <- function(x, mapper = NULL, n = 3){
  UseMethod('ngram')
}

ngram.character <- function(x, mapper = NULL, n = 3){
  if (!is.null(mapper)){
    x <- mapper(x)
  }
  t <- unlist(strsplit(x, split = ""))
  t <- c(rep(" ", n), t, rep(" ", n))
  colNum <- n * 2 + 1
  x <- lapply(seq_len(colNum), function (i) t[seq_len(length(t) - colNum + 1) - 1 + i])
  out <- data.frame(x, stringsAsFactors = FALSE)
  if (n == 0){
    names(out) <- "N0"
  } else {
    names(out) <- c(paste0("N_",n:1), "N0", paste0("N", 1:n))
  }
  out
}

ngram.default <- function(x, mapper = NULL, n = 3){
  ngram(as.character(x), mapper = mapper, n = n)
}


ngram.VCorpus <- function(x, mapper = NULL, n = 3){
  stopifnot(inherits(x, 'VCorpus') && all(vapply(x, function(y) inherits(y, 'TextDocument'), logical(1))))
  Reduce(rbind, lapply(x, function (d) ngram(d, mapper, n)))
}


#' Map characters using a list (internal)
#'
#' Convert characters to character class specified by a list.
#'
#' This function convert non-thai character to A for alphabets, D for digits, and
#' P for punctuations. Then it uses the specified list to map characters. Then to catch
#' other remaining characters, it converts non-alphabets, except space to P. Spaces
#' are left as is, unless they are supplied in the cpm map.
#'
#' Note: Private function. Intended to be used internally.
#'
#' @param x character
#' @param cpm list containing the mapping
#'
#' @return character
#'
#' @examples
#' \dontrun{
#'  listMapper('abc !# 123', list())
#' }
listMapper <- function(x, cpm){
  x <- gsub("[[:alpha:]]", "A", x)
  x <- gsub("[\u0e51\u0e52\u0e53\u0e54\u0e55\u0e56\u0e57\u0e58\u0e59\u0e50[:digit:]]", "D", x)
  x <- gsub("[[:punct:]]", "P", x)
  charV <- unlist(strsplit(x, split = ""))
  mapped <- cpm[charV]
  nullF <- vapply(mapped, is.null, logical(1))
  mapped[nullF] <- charV[nullF]
  x <- paste0(mapped, collapse = "")
  # by this line everything should be represented by a single character or a space
  # catch all
  x <- gsub("[[:space:]]", ' ', x)
  gsub("[^[:alpha:][:space:]]", "P", x)
}

#' Ngram model with multiple mappers
#'
#' Note: intended to be used internally.
#' Same as ngram, but accept multiple mappers.
#'
#' @param x input
#' @param mappers list of mapper
#' @param n number of gram
#' @return data frame
#'
ngramModel <- function(x, mappers, n = 3){
  stopifnot(inherits(x, 'character') || inherits(x, 'VCorpus'))
  Reduce(cbind, lapply(seq_along(mappers), function(i){
    df <- ngram(x, mapper = mappers[[i]], n = n)
    names(df) <- paste0("F", i, names(df))
    df
  }))
}


# specific 3-gram model, default mappers, default levels
char2modelInput <- function(x){
  df <- ngramModel(x, defaultMappers)
  df[1:7] <- lapply(df[1:7], function(y) factor(y, levels = defaultLevels[[1]]))
  df[8:14] <- lapply(df[8:14], function(y) factor(y, levels = defaultLevels[[2]]))
  df
}


