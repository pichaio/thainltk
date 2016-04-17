#' Find all possible words at each character position in string
#'
#' @param str input string
#' @param trie dictionary trie
#' @param maxChar max length of words to find, optional.
#' @param words list of words in trie, optional. Used to speed up search algo.
#'
#' @return list of character vectors, each containg possible words at the position.
#'
allPossibleSpans <- function(str, trie, maxChar = NULL, words = NULL){
  #phrases <- strsplit(str, '[[:space:]]+')
  chara <- unlist(strsplit(str, ''))

  if (is.null(words)){
    if (is.null(maxChar)) {
      lapply(seq_along(chara), function(i) trie$findAll(chara[i:length(chara)]))
    } else {
      lapply(seq_along(chara), function(i) trie$findAll(chara[i:min(length(chara), i + maxChar)]))
    }
  } else {
    # for faster computation, match known, valid, bi- and tri- gram characters
    # if doesn't match, no need to look further
    knownTwoChar <- substr(words, 1, 2)
    knownThreeChar <- substr(words, 1, 3)
    if (is.null(maxChar)){
      maxChar <- max(nchar(words))
    }
    # bi and tri gram
    cbg <- paste0(chara[-length(chara)], chara[-1])
    ctg <- paste0(cbg[-length(cbg)], chara[-c(1, 2)])
    hcbg <- match(cbg, knownTwoChar)
    hctg <- c(match(ctg, knownThreeChar), NA)
    valcbg <- match(cbg, words)

    k <- lapply(which(!is.na(hcbg) & !is.na(hctg)), function(i) trie$findAll(chara[i:min(length(chara), i + maxChar)]))
    out <- lapply(seq_along(chara)[-1], function (x) character(0))
    out[!is.na(hcbg) & !is.na(hctg)] <- k
    out[!is.na(valcbg) & is.na(hctg)] <- cbg[!is.na(valcbg) & is.na(hctg)]
    out[[length(out) + 1]] <- character(0)
    out
  }
}

