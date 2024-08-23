#' Reorder the rows of a dataframe by factors' level
#'
#' @description
#' Find a permutation (stochastic) of the row indices of a dataframe with the property that rows `perm[1:n]` are roughly balanced according to the levels of the factors, for not-small `n`.
#' * If `frontLoad = FALSE` the levels in `perm[1:n]` are roughly representative of the proportions in the dataframe, for not-small `n`.
#' * If `frontLoad = TRUE` the levels in `perm[1:n]` are roughly balanced for not-small and not-large `n`: levels which are less common run out more quickly, and do not appear at the end of `perm`.
#'
#' @param X A dataframe which may have factor variables.
#' @param frontLoad Logical, have roughly balanced numbers of each level at the start of the reorder.
#'
#' @details Internally, the factors are processed sequentially ordered by the number of levels, so that when `frontLoad = TRUE` the factors with small numbers of levels are not pushed down the reordering.
#'
#' @returns A permutation of the rows indices of `X`.
#'
#' @examples
#' ## Iris dataframe is balanced
#'
#' p0 <- reorder_rows(iris)
#' show(iris[p0, ])
#'
#' ## add another factor to Dobson (1990) from ?lm
#'
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' X <- data.frame(weight = c(ctl, trt), group)
#' dow <- rep(c("Mon", "Tue", "Wed"), c(12, 5, 3))
#' X$dow <- factor(dow)
#'
#' ## representative
#' p1 <- reorder_rows(X) # frontLoad = FALSE
#' show(X[p1, ])
#'
#' ## balanced
#' p2 <- reorder_rows(X, frontLoad = TRUE)
#' show(X[p2, ])
#'
#' @export

reorder_rows <- function(X, frontLoad = FALSE) {

  stopifnot(is.data.frame(X))
  n <- nrow(X)
  fac <- sapply(X, is.factor)
  if (!any(fac)) {
    warning("No factors, returning random permutation")
    return(sample.int(n, n))
  }

  ## order the factors by number of levels

  fac <- which(fac)
  fac[] <- fac[order(sapply(X[fac], nlevels))]

  ## get the rows of each level of each factor

  Z <- lapply(X[fac], function(x) {
    robj <- lapply(levels(x), function(lev) {
      y <- which(as.character(x) == lev)
      m <- length(y)
      y[] <- y[sample.int(m, m)] # permute
      z <- rep(NA_integer_, n)
      if (isTRUE(frontLoad)) {
        z[1L:m] <- y
      } else {
        delta <- n / m
        ii <- seq(from = 1, by = delta, length.out = m)
        ii[] <- floor(ii + delta / 2)
        z[ii] <- y
      }
      z
    })
    do.call("cbind", robj)
  })

  ## stack these by column and then process by row

  Z <- do.call("cbind", Z)
  Z <- as.vector(t(Z)) # column major order
  Z <- unique(Z[!is.na(Z)])

  stopifnot(identical(sort(Z), 1L:n)) # just in case!
  Z
}

