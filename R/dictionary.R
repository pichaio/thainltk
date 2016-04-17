

#' Dictionary trie
#'
#' Note: Private function. Intended to be used internally.
#' The dictionary trie is useful when querying prefix matching.
#' This dictionary can help creating expressive features. For example, it can tell
#' whether a particular sequence of character is a possible word begining. When the
#' dictionary contains reversed words, it can tell whether a particular sequence is
#' a possible word end.
#'
#'
#' @return the dictionary
#'
#' @examples
#' \dontrun{
#' d <- createDict()
#' d$add('abc')
#' d$contains('abc')
#' d$validPrefix('ab')
#' }
createDict <- function(){
  rootnode <- list()
  walkTree <- function(word, build=FALSE){
    ischara <- is.atomic(word) && is.character(word) && all(nchar(word) == 1)
    stopifnot((length(word) == 1 && is.character(word)) ||
                ischara)
    chars <- if (ischara) word else strsplit(word, "")[[1]]
    path <- character()
    for (char in chars){
      path <- c(path, char)
      if (is.null(rootnode[[path]])){
        if (build) rootnode[[path]] <<- list() else return(NULL)
      }
    }
    path
  }

  findAll <- function(word){
    ischara <- is.atomic(word) && is.character(word) && all(nchar(word) == 1)
    stopifnot((length(word) == 1 && is.character(word)) ||
                ischara)
    chars <- if (ischara) word else strsplit(word, "")[[1]]
    path <- character()
    out <- character(0)
    cnt <- 0
    for (char in chars){
      cnt <- cnt + 1
      path <- c(path, char)
      indict <- rootnode[[path]]
      if (is.null(indict)){
        return(out)
      } else {
        if (!is.null(attr(indict,'end'))){
          out <- c(out, paste0(chars[1:cnt], collapse = ''))
        }
      }
    }
    out
  }


#' Aadd a new word
#'
#' @param word single character
#'
#' @return none
  add <- function(word){
    path <- walkTree(word, build=TRUE)
    attr(rootnode[[path]],'end') <<- TRUE
  }
#' Check if a word is in the dictionary
#'
#' @param word
#'
#' @return logical
  contains <- function(word){
    path <- walkTree(word, build=FALSE)
    !is.null(path) && (length(path) == 0 || !is.null(attr(rootnode[[path]],'end')))
  }
#' Check if a character sequence can begin a word
#'
#' @param word character
#'
#' @return logical
  validPrefix <- function(word, trim = T){
    if (trim){
      trimmed <- trimws(word, which = 'right')
      if (!identical(trimmed, word)){
        return(contains(trimmed))
      }
    }
    path <- walkTree(word, build=FALSE)
    !is.null(path)
  }
  structure(list(add = add, contains = contains, validPrefix = validPrefix, findAll = findAll),
            class='TreeDict')
}


