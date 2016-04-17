library(tm)
library(hash)
library(parallel)
library(Matrix)
setname <- 'train'

cor <- readRDS(sprintf('data-raw/best%s.RDS', setname))
#trainf <- createFeaturesV2(cor[[1]]$content)

cl <- makeCluster(3)
clusterEvalQ(cl, {
  library(tm)
  library(hash)
  library(Matrix)
  devtools::load_all(".")
  NULL
})

system.time({
  alls <- parLapply(cl, cor, function (x) createFeaturesV2(x$content))
})

clabels <- as.integer(cut(seq_along(alls), 6, seq(6)))
allm <- lapply(alls, features2Matrix)
#c1 <- do.call(cbind, allm[clabels == 1])
rm(alls)
cn <- lapply(seq(6), function (i) do.call(cbind, allm[clabels == i]))
X <- do.call(cbind, cn)
#X <- cbind(c1, X)
saveRDS(X, sprintf('../data/%s_f2_1_all_X.RDS', setname))
y <- lapply(cor, createLabels)
y <- do.call(c, y)
y[y == 1] <- -1L
y[y == 2] <- 1L
y <- unname(y)
saveRDS(y, sparintf('../data/%s_f2_1_all_y.RDS', setname))
