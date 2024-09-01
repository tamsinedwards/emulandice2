#' SVD Imputation Algorithm for Matrix Completion
#' Code by Jonty Rougier
#'
#' @description
#' Replace `NA` values in matrix `X` with imputed values.  Order `X` with time or equivalent down the rows, because the imputation is done row by row.
#'
#' Although `k` can be specified directly, it is safer to set it indirectly using `pmin`.
#'
#' @param X Numeric matrix containing missing values as `NA`.
#' @param k Desired rank.
#' @param pmin Minimum proportional variation, used to set `k` if `k = NULL`, see Details.
#' @param reltol Relative tolerance for exit condition, see Details.
#' @param maxit Maximum number of iterations, see Details.
#'
#' @details The algorithm exits if:
#'
#' 1. There has been `maxit` iterations.
#' 2. The norm of the change in the imputed values starts to rise.
#' 3. The norm of the change in the imputed values falls below `reltol` times the norm of `X` (after scaling).
#'
#' If `k` is not specified, the value of `k` is calculated from the SVD of `X` after it has been centered and scaled and `NA` values have been imputed using row means.  `k` is the smallest value for which the proportion of variation is at least `pmin`.
#'
#' @returns A matrix like `X` but with the `NA`s imputed, plus an attributes `k` and `niter`, the number of iterations.
#'
#' @references Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown, Trevor Hastie, Robert Tibshirani, David Botstein, Russ B. Altman, Missing value estimation methods for DNA microarrays, Bioinformatics, Volume 17, Issue 6, June 2001, Pages 520–525, <https://doi.org/10.1093/bioinformatics/17.6.520>.
#'
#' @export

SVDimpute <- function(X, k = NULL, pmin = 0.99, reltol = 1E-3, maxit = 20) {

  stopifnot(is.matrix(X), is.numeric(X), is.scalar(reltol), is.scalar(maxit))
  if (!anyNA(X)) {
    warning("No missing values")
    return(X)
  }

  ## into differences

  dn <- dimnames(X)
  first_row <- X[1L, ]
  X0 <- X <- apply(unname(X), 2L, diff) # save NAs in X0

  repack <- function(X) {
    X <- apply(rbind(first_row, X), 2L, cumsum)
    dimnames(X) <- dn
    X
  }

  ## initial fill

  last_all <- which.max(apply(is.na(X), 1L, any)) - 1L
  data <- data.frame(y = as.vector(X), row = as.factor(row(X)))
  data$offset <- X[last_all, col(X)]
  na <- is.na(data$y)
  lmo <- lm(y ~ row, offset = offset, data = data[!na, ])
  pp <- predict(lmo, newdata = data[na, ])
  X[na] <- pp

  if (maxit < 1) {
    return(repack(X))
  }
  maxit[] <- ceiling(maxit)

  ## estimate rank from filled X

  if (is.null(k)) {
    stopifnot(is.scalar(pmin), 0 < pmin, pmin < 1)
    decomp <- svd(X, nu = 0, nv = 0) # no NAs in X
    d <- decomp$d^2
    d <- cumsum(d) / sum(d)
    k <- which.max(d >= pmin)
  } else {
    stopifnot(is.pos.int(k))
  }

  ## iterate row completion

  na <- which(is.na(X0), arr.ind = TRUE)
  ivals <- unique(na[, 1L])
  jvals <- lapply(ivals, function(i) {
    na[na[, 1L] == i, 2L]
  })

  ## exit criterion

  frob <- function(X) {
    sqrt(sum(X * X))
  }
  f0 <- frob(X)
  na <- is.na(X0)
  newimp <- imp <- X[na]
  newdd <- dd <- Inf

  ## here we go

  for (iter in seq.int(maxit)) {
    decomp <- svd(X, nu = 0, nv = k) # thin SVD
    xx <- decomp$v # n by k
    for (i in ivals) {
      for (j in jvals[[match(i, ivals)]]) {
        coff <- qr.coef(qr(xx[-j, , drop=FALSE]), X[i, -j]) # k vector
        X[i, j] <- drop(crossprod(xx[j, ], coff))
      }
    }
    newimp[] <- X[na]
    if (anyNA(newimp)) {
      stop("your \'k\' is too large")
    }
    newdd[] <- frob(imp - newimp)
#    show(c(dd, newdd)) # This prints iteration to screen
    if (newdd > dd) {
      X[na] <- imp # reset
      break
    }
    if (newdd < reltol * (f0 + reltol)) {
      break
    }
    imp[] <- newimp
    dd[] <- newdd
  }

  X <- repack(X)
  attr(X, "k") <- k
  attr(X, "niter") <- iter
  X
}

