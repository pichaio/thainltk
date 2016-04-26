#' Sample documents from BEST corpus
#'
#' Three sample documents from BEST corpus. Need to be unencoded before use. See
#' details.
#'
#'
#' BEST, Benchmark for Enhancing the Standard of Thai Language Processing,
#' corpus is a collection of texts used for NLP processing competitions. This
#' particular corpus was used in Thai Word Segmentation competition in 2010. The
#' corpus contains texts derived from 4 sources: article, news, encyclopedia,
#' and novel. The data are distributed by National Electronics and Computer
#' Technology Center (NECTEC) under Creative Common 3.0 license.
#'
#' This small BEST corpus contains randomly-selected 3 documents out of the
#' entire corpus. The complete set of documents can be obtained by downloading
#' from the source sites (free registration is required). Once downloaded and
#' unzipped, \code{\link{BESTDocument}} and \code{\link{BESTCorpus}} can be used
#' to parse the documents and obtain VCorpus object.
#'
#' For portability reason, this sbest dataset is unicode-escaped. To use the
#' dataset, unescape it by calling \code{\link{unescapeSbest}}, instead of
#' calling data(sbest).
#'
#' @format a tm::VCorpus object. The document is a
#'   NLP::AnnotatedPlainTextDocument
#'
#' @source \url{http://www.nectec.or.th/corpus/index.php?league=pm}
"sbest"


#' Unescape sbest dataset
#'
#' Unescape the sbest dataset.
#'
#'
#' @return tm::VCorpus of AnnotatedPlainTextDocument
#' @export
#'
#' @examples
#' library(tm)
#' sbest <- unescapeSbest()
unescapeSbest <- function(){
  stopifnot(requireNamespace('tm'))
  data('sbest', envir = environment())
  for (i in seq_along(sbest)){
    sbest[[i]]$content <- NLP::as.String(stringi::stri_unescape_unicode(sbest[[i]]$content))
  }
  sbest
}
