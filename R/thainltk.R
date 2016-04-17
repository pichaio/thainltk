#' thainltk: Thai National Language Toolkit
#'
#' This package is intended to provide utility functions for Thai NLP. The first
#' release includes an SVM-based Thai word tokenizer, trained on publicly
#' available BEST corpus.
#'
#'
#' This product is created by the adaptation of LEXiTRON developed by NECTEC
#' \url{http://www.nectec.or.th/}
#'
#' @docType package
#' @name thainltk
NULL

.onLoad <- function(libname, pkgname){
  wordTrie <- createDict()
  invisible(lapply(knownWords, wordTrie$add))
  svmparam <- readBin(svmpraw, 'double', n = 200000, size = 4)
  mapper <- function(charMap){
    force(charMap)
    function(x){
      listMapper(x, charMap)
    }
  }

  mapper1 <- mapper(charClass1)
  mapper2 <- mapper(charClass2)
  defaultMappers <- list(mapper1, mapper2)

  assign('wordTrie', wordTrie,
         envir = asNamespace(pkgname))
  assign('svmparam', svmparam,
         envir = asNamespace(pkgname))
  assign('defaultMappers', defaultMappers,
         envir = asNamespace(pkgname))
}
