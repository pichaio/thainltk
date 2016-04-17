#' Partial SVM cost and gradients function
#'
#' Intended to be used internally
#' Linear SVM cost is comprised of the L2-norm of W and empirical loss.
#' This function calculates only the empirical loss. This loss then can
#' be calculated on clusters, each holding a different segment of training
#' data. It then can be aggregated at the master node.
#'
#' @param theta list of parameters. Expect the first element to be a
#' list, having w and b as elements.
#' @param data matrix p by n. p is the number of features and n is the
#' number of training observations.
#' @param labels integer vector containing 1 or -1, for class identification.
#' @param C regularization parameter. The greater the more regularized.
#'
#' @return list of cost and gradients.
partialSvmCost <- function(theta, data, labels, C){
  ybar <- as.vector(theta[[1]]$w %*% data + theta[[1]]$b)
  cost <- C * sum(pmax(0, 1 - labels * ybar))
  sv <- ybar * labels < 1
  grad <- list()
  dlbdw <- t(data[, sv] %*% labels[sv])
  grad[[1]] <- list(
    w = - C * dlbdw,
    b = - C * sum(labels[sv])
  )
  list(cost = cost, grad = grad)
}

#' Parallel SVM
#'
#' Prepare gradient and cost functions, to be passed to an optimizer. It sends
#' parameters to cluster nodes and recieve partial cost and gradients back. It
#' then calculates total cost and gradients.
#'
#' @param cl clusters
#' @param C SVM regularized parameter
#'
#' @return a list of cost and gradient functions. To be passed to optim
#'
parSvm <- function(cl, C){
  force(cl)
  parallel::clusterExport(cl, c('C'), envir = environment())
  results <- NULL
  parCost <- function(p){
    theta <- list(list())
    theta[[1]]$b <- p[[1]]
    theta[[1]]$w <- matrix(p[-1], nrow = 1)
    parallel::clusterExport(cl, 'theta', envir = environment())
    results <<- parallel::clusterEvalQ(cl, partialSvmCost(theta, data, labels, C))
    cost <- Matrix::tcrossprod(theta[[1]]$w)/2 + sum(vapply(results, function (l) l$cost, numeric(1)))
    cost
  }

  parGrad <- function(p){
    grads <- Reduce(function (g1, g2){
      list(list(
        w = g1[[1]]$w + g2[[1]]$w,
        b = g1[[1]]$b + g2[[1]]$b
      ))
    }, lapply(results, function (x) x$grad))
    grads[[1]]$w <- matrix(p[-1], nrow = 1) + grads[[1]]$w
    c(grads[[1]]$b, as.vector(grads[[1]]$w))
  }
  list(cost = parCost, grad = parGrad)
}

#' Prepare Clusters for Parallel SVM Training
#'
#' The input matrix and labels will be divided evenly and shipped to clusters.
#' Appropriate libraries will be loaded and the partial cost function will be
#' defined.
#'
#'
#' @param cl clusters, perhaps created by makeClusters
#' @param data matrix p by n. p is the number of features and n is the
#' number of training observations.
#' @param labels integer vector containing 1 or -1, for class identification.
#'
#' @return nothing
#'
prepareCl4Svm <- function(cl, data, labels){
  ncores <- length(cl)
  sidx <- seq_len(ncol(data))
  cuts <- as.integer(cut(sidx, ncores))
  cutData <- lapply(seq_len(ncores), function (i) data[,sidx[cuts == i]])
  cutLabels <- lapply(seq_len(ncores), function (i) labels[sidx[cuts == i]])

  parallel::clusterEvalQ(cl, {
    library(Matrix)
    NULL
  })
  for (i in seq_len(ncores)){
    data <- cutData[[i]]
    labels <- cutLabels[[i]]
    parallel::clusterExport(cl[i], c('data', 'labels'), envir = environment())
  }
  parallel::clusterExport(cl, c('partialSvmCost'), envir = environment())
}
