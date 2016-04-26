
#' Thai Tokenizer
#'
#' Obtain a Thai tokenizer closure.
#'
#' The tokenizer is a closure that takes character as input and return a
#' character vector containing word tokens. If the input is a character vector,
#' the strings will be concatenated by a single space and tokenized.
#'
#' The tokenizer is based on an SVM model, trained on BEST corpus. The sample
#' documents of BEST corpus are made available in this package too. See
#'  \code{\link{sbest}}.
#'
#' The model takes around 100,000 input features. The features can be grouped
#' into 3 different types.
#' \enumerate{
#' \item Possible words at and after the current position.
#' \item Character bigrams at and after the current positions.
#' \item Cluster id of the current character, 3 character before and 3 character
#' after the current position. The clusters were discovered by an hierarchical
#' clustering method. The similarity is calculated from statistics of characters
#' near begin and end of words from Lexitron dictionary
#' }
#'
#' The performance of this tokenizer should be competitive with other recent models.
#' The F1 measure on a holdout test set is around 0.978.
#'
#'
#' @param skipSpace logical. To emit space as token or not.
#'
#' @return a closure that can tokenize thai text
#' @export
#'
#' @examples
#' tok <- thaiTokenizer()
#' tok('thai text to be tokenized')
thaiTokenizer <- function(skipSpace = T){
  force(skipSpace)
  svmtokenizer <- function(s){
    if (!is.character(s)) s <- as.character(s)
    if (is.na(s) || is.null(s) || identical(s, character(0))) return(s)
    if (is.vector(s) && length(s) > 1){
      s <- paste(s[!(is.na(s) || is.null(s) || vapply(s, function (y) identical(y, character(0)), logical(1)))], collapse = "\n")
    }
    if (!stringi::stri_enc_isutf8(s)) {
      # on Windows, the OS might be set to use Thai natively. The default string
      # is not UTF-8, but Windows-874. Need to convert to UTF-8.
      # on other OS, it should work too.
      s <- enc2utf8(s)
    } else {
      # on non-utf8 locale (e.g. C, POSIX)
      # the string might be encoded in UTF-8 but marked as unknown
      # if not marked properly, the system will treat each byte separately.
      if (Encoding(s) != 'UTF-8'){
        Encoding(s) <- 'UTF-8'
      }
    }
    if (nchar(s) < 2) return(s)

    X <- features2Matrix(createFeaturesV2(s))
    pred <- (as.vector(svmparam[-1] %*% X + svmparam[[1]]) > 0)
    out <- splitByPos(s, which(pred == 1))
    if (skipSpace){
      out <- out[grepl('[^[:space:]]', out)]
    }
    out
  }
}
