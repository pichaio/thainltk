library(tm)
library(hash)
library(parallel)
library(Matrix)

data(words)
cor <- readRDS('data-raw/besttest.RDS')
maxnchar <- max(nchar(words))
data(words)
trie <- createDict()
invisible(lapply(words, trie$add))
alls <- allPossibleSpans(cor[[1]]$content, trie, 52L)

cl <- makeCluster(3)
clusterEvalQ(cl, {
  library(tm)
  devtools::load_all(".")
  data(words)
  trie <- createDict()
  invisible(lapply(words, trie$add))
  NULL
})

alls <- parLapplyLB(cl, cor, function (x) allPossibleSpans(x$content, trie, 52L))
saveRDS(alls, '../data/test_wordspan.RDS')
allv <- readRDS('../data/val_wordspan.RDS')

allwords1 <- unique(unlist(lapply(alls, unlist)))
allwords2 <- unique(unlist(lapply(allv, unlist)))
alltr <- readRDS('../data/train_wordspan.RDS')
allwords3 <- unique(unlist(lapply(alltr, unlist)))

knownWords <- unique(c(allwords1, allwords2, allwords3))
