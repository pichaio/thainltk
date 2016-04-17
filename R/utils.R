
#' Split char by specified positions
#'
#' @param x char
#' @param endIdx integer vector, end postions
#'
#' @return character vector
splitByPos <- function(x, endIdx){
  if (length(endIdx) == 0) return(x)
  beginIdx <- c(1, endIdx[-length(endIdx)] + 1)
  tokens <- unlist(Map(function(b, e){
    substr(x, b, e)
  }, beginIdx, endIdx))
  tokens
}
