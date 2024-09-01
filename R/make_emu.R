#' make_emu: build emulator
#'
#' @description
#' Build emulators of principal components with RobustGaSP.
#' Code originally by Jonty Rougier.
#'
#' @returns `make_emu()` returns an emulator object to use.
#'
#' @export


# ________________----
# EMULATE ------------------------------------------------------------

# Build emulator -----------------------------------------------------------------------

make_emu <- function(designX, responseF, r = NULL, thresh = 0.999) {

  # ARGUMENTS WHEN CALLED:
  #    designX <- ice_design_scaled
  #    responseF <- as.matrix( ice_data[ , paste0("y", years_em) ] )
  #    r <- NULL
  #    thresh <- 0.99

  cat("_____________________________________\n", file = logfile_build, append = TRUE)
  cat("make_emu: building emulator...\n", file = logfile_build, append = TRUE)

  stopifnot(is.matrix(designX))
  m <- nrow(designX)
  d <- ncol(designX)
  stopifnot(is.matrix(responseF), nrow(responseF) == m)
  n <- ncol(responseF)
  if (!is.null(r))
    stopifnot(r == round(r), 0 < r, r <= n)
  stopifnot(length(thresh) == 1, 0 < thresh, thresh < 1)

  ## SVD

  cc <- colMeans(responseF)

  # Use sweep to centre data (subtract column means from columns of responseF)
  # then do SVD i.e. a PCA
  decomp <- svd(sweep(responseF, 2L, cc, "-"))
  dd2 <- decomp$d^2
  scree <- cumsum(dd2) / sum(dd2)
  if (is.null(r))
    r <- which.max(scree >= thresh) # first exceedance
  U <- decomp$u[, 1L:r, drop=FALSE]
  Vt <- (decomp$d * t(decomp$v))[1L:r, , drop=FALSE]

  ## write a message

  cat(sprintf("make_emu: r = %i, scree = %.1f%%\n", r, 100 * scree[r]), file = logfile_build, append = TRUE)

  ## build emulators, hide rgasp output

  sink(file = paste0(outdir,out_name,"_rgasp.log"))

  # Get all inputs
  trendX <- designX

  # Drop factors (dummy variable columns) from trend
  if (include_factors) {

    cat("\nDropping factors from trend:\n", file = logfile_build, append = TRUE)
    cat(paste(c(ice_dummy_list, "\n"), collapse = " "), file = logfile_build, append = TRUE)

    trendX <- trendX[ , input_cont_list]

    cat("\nKeeping:\n", file = logfile_build, append = TRUE)
    cat(paste(c(colnames(trendX), "\n"), collapse = " "), file = logfile_build, append = TRUE)
  }

  EMU <- lapply(1L:r, function(j) {

    RobustGaSP::rgasp(design = designX, response = U[, j], trend = cbind(1, trendX),
                      nugget.est = TRUE , lower_bound = lower_bound,
                      kernel_type = kernel, alpha = rep(alpha, dim(as.matrix(designX))[2]))
  })

  sink()

  ## predict method returns a list

  pred_EMU <- function(designXout) {

    trendXout <- designXout

    # Drop any factors from trend
    if (include_factors) {
      tt <- which( input_cont_list %in% colnames(ice_design), arr.ind = TRUE )
      trendXout <- trendXout[ , tt, drop = FALSE]
    }

    lapply(EMU, function(emu) {
      RobustGaSP::predict(emu, testing_input = designXout,
                          testing_trend = cbind(1, trendXout))[c("mean", "sd")]
    })
  }

  ## return a function

  robj <- function(designXout, type = c("mean", "sd", "var", "all")) {

    type <- match.arg(type)
    if (!is.matrix(designXout) && length(designXout) == d) {
      dim(designXout) <- c(1L, d)
    } else {
      stopifnot(is.matrix(designXout), ncol(designXout) == d)
    }
    m_out <- nrow(designXout)
    pplist <- pred_EMU(designXout) # r-list

    ## compute the mean

    mu <- sapply(pplist, "[[", "mean") # m_out x r
    dim(mu) <- c(m_out, r)
    mx <- sweep(mu %*% Vt, 2L, cc, "+") # m_out x n
    if (type == "mean")
      return(list(mean = mx))

    ## compute the sd

    sdu <- sapply(pplist, "[[", "sd") # m_out x r
    dim(sdu) <- c(m_out, r)

    sdx <- lapply(1L:m_out, function(i) {
      sqrt(colSums((sdu[i, ] * Vt)^2)) # n vector
    })
    sdx <- do.call("rbind", sdx) #  m_out x n
    if (type == "sd") return(list(mean = mx, sd = sdx))

    ## compute the variance; TLE changed to return sd too

    Sx <- lapply(1L:m_out, function(i) {
      as.vector(crossprod(sdu[i, ] * Vt)) # n*n vector
    })
    Sx <- do.call("cbind", Sx) # n*n x m_out
    dim(Sx) <- c(n, n, m_out)
    Sx <- aperm(Sx, c(3, 1, 2)) # I wonder who wrote aperm :)
    return(list(mean = mx, sd = sdx, var = Sx))
  }

  ## class and return

  cat("make_emu: end of emulator build\n",file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  structure(robj, class = "emu")

}


# END MAKE_EMU()
