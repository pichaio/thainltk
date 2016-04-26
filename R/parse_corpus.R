

#' Parse document from BEST corpus (internal)
#'
#' Parse a text file from BEST corpus to AnnotatedPlainTextDocument.
#'
#' Each text file from BEST corpus contains manually segmented words, seperated by pipe ('|').
#' Sentence boundaries can be identified by a single space between two pipes.
#' Line ends may or may not be sentence boundaries. Some of special words were tagged by xml-style tags, such as <NE>, <AB>, etc.
#'
#' The returned AnnotatedPlainTextDocument contains annotations with 'word', 'NE' and 'AB' types. See
#' NLP::Annotation, and annotations for more information
#'
#' Note: Private function. Intended to be used internally.
#'
#' @param con a connection object or a character string.
#' @param fileName will become the id of document
#' @param category should be one of for sources May be used for classification.
#' @param sep word seperator. Should be a single character.
#' The default is '|'.
#' @param ignores do not include tokens start with any of specified string.
#' @param removeTags remove xml-style open and close tags from token.
#' @param ... further arguments that will be passed to readLines
#'
#'
#' @return AnnotatedPlainTextDocument
#'
BESTDocument <- function(con, fileName, category, sep="|", ignores=character(0), removeTags=TRUE, ...){
  allText <- paste0(readLines(con, ...), collapse = '')
  splitted <- strsplit(allText, split = '|', fixed = T)[[1]]

  # tagged tokens
  ne <- grepl("^<NE>.*</NE>$", splitted)
  ab <- grepl("^<AB>.*</AB>$", splitted)
  spaces <- grepl("^[[:space:]]+$", splitted)
  csplitted <- gsub("(^<[[:alnum:]]+>|</[[:alnum:]]+>$)", "", splitted)


  charCnt <- nchar(csplitted)
  tEnds <- as.integer(cumsum(charCnt))
  tBegins <- c(1L, tEnds[-length(tEnds)] + 1L)
  wordEnds <- tEnds[!spaces]
  wordBegins <- tBegins[!spaces]

  wa <- NLP::Annotation(seq_along(wordBegins), rep('word', length(wordBegins)),
                   wordBegins, wordEnds)
  nea <- NLP::Annotation((seq_len(sum(ne)) + length(wa)), rep('NE', sum(ne)),
                    tBegins[ne], tEnds[ne])

  aba <- NLP::Annotation((seq_len(sum(ab)) + length(wa) + length(nea)), rep('AB', sum(ab)),
                    tBegins[ab], tEnds[ab])
  alla <- c(wa,nea, aba)

  at <- NLP::AnnotatedPlainTextDocument(paste0(csplitted, collapse = ""),
                                   annotations = alla,
                                   meta = list(
                                     id = sub('\\.txt$', '', fileName),
                                     language = "th",
                                     origin = "BEST Corpus",
                                     category = category
                                   ))
  class(at) <- c('BESTDocument', class(at))
  at
}

#' Parse multiple documents from BEST corpus (internal)
#'
#' Parse a folder containing text files from BEST corpus
#'
#' Text files in BEST corpus are organized into 4 folders, based on their sources.
#' This function reads all text files in the specified folder and create a corpus object.
#' The corpus object is an S3 object defined in tm package.
#'
#' Note: Private function. Intended to be used internally.
#'
#' @param dirname input directory name
#' @param category source of files, should be one of article, news, novel, and encyclopedia.
#' @param ... additional parameters that will be passed to BestDocument. Typically encoding
#' = 'UTF-8' should be specified.
#'
#' @return tm's Corpus object.
#'
#' @examples
#' \dontrun{
#'  BestCorpus("data/article", "article", encoding = "UTF-8")
#' }
#'
BESTCorpus <- function(dirname, category, ...){
  docs <- lapply(dir(dirname), function(fname){
    f <- file(file.path(dirname, fname))
    on.exit(close(f))
    BESTDocument(f, fileName = fname, category = category, ...)
  })
  corpus <- tm::VCorpus(tm::VectorSource(character(0)))
  for (d in docs){
    corpus[[length(corpus)+1]] <- d
  }
  corpus
}




#' Parse Lexitron document (internal)
#'
#' Parse a document from Lexitron corpus.
#'
#' The disributed file doesn't strictly conform to XML standards. So standard
#' XML parser may not work well with the file. Two issues found are: 1. no root
#' node, and 2. no special character escaping, in particular &, <, and > were
#' used as is.
#'
#' Note: Private function. Intended to be used internally.
#'
#' @param filename lexitron file
#'
#' @return a data frame, each row for a word in the dictionary and each column
#'   for each tag.
#'
#' @examples
#' \dontrun{
#'  parseLexitron("lexitron-data/telex")
#' }
parseLexitron <- function(filename){
  con = file(filename, encoding='windows-874')
  on.exit(close(con))
  lines <- gsub("^<([[:alpha:]]+)>(.*)</([[:alpha:]]+)>","\\1|\\2", readLines(con))
  docBegins <- grep("<Doc>", lines)
  docEnds <- grep("</Doc>", lines)
  docs <- list(
    id = character(length(docBegins)),
    tsearch = character(length(docBegins)),
    tentry = character(length(docBegins)),
    eentry = character(length(docBegins)),
    tcat = character(length(docBegins)),
    tsyn = character(length(docBegins)),
    tsample = character(length(docBegins)),
    tdef = character(length(docBegins)),
    tant = character(length(docBegins)),
    tenglish = character(length(docBegins)),
    tnum = character(length(docBegins)),
    notes = character(length(docBegins))
  )
  Map(function(begin, end, i){
    x <- strsplit(lines[begin:end],split="|",fixed = T)
    for (y in x){
      docs[[y[[1]]]][[i]] <<- y[[2]]
    }
  }, docBegins+1, docEnds-1, seq_along(docBegins))
  df <- as.data.frame(docs, stringsAsFactors = F)
  df$id <- as.integer(df$id)
  df
}


