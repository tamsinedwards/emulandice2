#' make_emu: build emulator
#'
#' @description
#' Build emulators of principal components with RobustGaSP.
#' Based on code by Jonty Rougier.
#'
#' @param designX Full dataset design
#' @param forcingF Full climate timeseries for SVD (optional)
#' @param responseF Full dataset response

#' @returns `make_emu()` returns an emulator object to use.
#'
#' @export


# ________________----
# EMULATE ------------------------------------------------------------

# Build emulator -----------------------------------------------------------------------

make_emu <- function(designX, responseF, forcingX, r = NULL, thresh = 0.999) {

  # ARGUMENTS WHEN CALLED:
  #    designX: e.g. ice_design_scaled
  #    responseF: e.g. as.matrix( ice_data[ , paste0("y", years_em) ] )
  #.   forcingX: e.g. temps_all
  #    r <- NULL
  #    thresh <- 0.99

  cat("_____________________________________\n", file = emu_log_file, append = TRUE)
  cat("make_emu: building emulator...\n", file = emu_log_file, append = TRUE)

  ## Put any miscellaneous output in log file
  sink(file = emu_log_file, append = TRUE)

  # Check design matrix - more checks are in check_design() before main call
  stopifnot(is.matrix(responseF))

  # Check response matrix
  n_inputs <- ncol(designX)
  stopifnot(is.matrix(responseF), nrow(responseF) == nrow(designX))
  n_times <- ncol(responseF)
  if (!is.null(r))
    stopifnot(r == round(r), 0 < r, r <= n_times)
  stopifnot(length(thresh) == 1, 0 < thresh, thresh < 1)

  # SVD -----------------------------------------------------------------------

  ## SVD of outputs

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

  pdf( file = paste0( outdir, out_name, "_SVD_SLE.pdf"),
       width = 9, height = 5)
  plot(1:length(scree), scree, type = "b", xlab = "Rank", ylab = "Total variance explained", pch = 20)
  abline(h=1)
  abline(h=thresh, lty = 3)

  for (j in 1L:r) {
    plot(years_em,Vt[ j, ], type = "l", xlab = "Time", ylab = paste("Singular value * right singular vector", j))
  }

  dev.off()

  ## write a message

  cat(sprintf("\nmake_emu build: SLE SVD r = %i, scree = %.3f%%\n", r, 100 * scree[r]), file = emu_log_file, append = TRUE)

  # Model selection -----------------------------------------------------------------------
  # Use lasso regression to drop inert inputs (beta coefficients zero)

  coef_tol <- 1e-12 # for selecting zero coefficients
  x_vars <- designX # xxx no need - replace
  x_names <- colnames(x_vars)

  # Categorical colours to plot beta for each PC
  beta_palette <- hcl.colors(r+1, palette = "Blues 3", alpha = 0.8)
  # Ditch white at end
  beta_palette <- beta_palette[1:r]

  # Loop through PCs, finding all active inputs with lasso regression of U[,j] ~ designX
  beta <- lapply(1L:r, function(j) {

    cat(sprintf("\nModel selection for PC%i\n\n", j), file = emu_log_file, append = TRUE)

    # First estimate regularisation penalties for all models using cross-validation
    # Linear models only (no interactions, squared terms); alpha = 1 is lasso
    cv_model <- glmnet::cv.glmnet(x_vars, U[,j], alpha = 1)

    # Plot all lambdas - shows minimum lambda and 1 s.e. from minimum
    pdf( file = paste0( outdir, out_name, "_lambda_CV_PC",j,".pdf"), width = 9, height = 5)
    plot(cv_model, main = paste0("Regularisation term for PC", j))
    dev.off()

    # Take minimum lambda as the best penalty value
    cat(sprintf("Best lambda for regularisation = %.6f\n\n", cv_model$lambda.min),
        file = emu_log_file, append = TRUE)

    # Now predict model using this lambda value to get coefficient estimates
    best_model <- predict(cv_model, x_vars, s = "lambda.min", type = "coefficients")
    all_coef <- best_model[ 2:(ncol(x_vars)+1), "lambda.min"]
    names(all_coef) <- x_names

    # Print all coefficients to log file
    cat("\nEstimated regression coefficients: \n\n", file = emu_log_file, append = TRUE)
    for (cc in 1:length(x_names)) {
      cat(x_names[cc],"\t", all_coef[cc], "\n", file = emu_log_file, append = TRUE)
    }
    cat("\n", file = emu_log_file, append = TRUE)

    # Names of inputs with non-zero coefficients
    keep_inputs_PC <- x_names[ abs(all_coef) > coef_tol ]
    cat(sprintf("Active terms for PC%i: \n", j), file = emu_log_file, append = TRUE)
    cat(keep_inputs_PC,"\n\n", file = emu_log_file, append = TRUE)

    cat(sprintf("Inert terms for PC%i: \n", j), file = emu_log_file, append = TRUE)
    cat(x_names[ ! x_names %in% keep_inputs_PC ],"\n\n", file = emu_log_file, append = TRUE)


    # Assign colours to symbols for plot
    sym_fill <- rep("lightgrey", length(x_names))
    sym_fill[ x_names %in% keep_inputs_PC ] <- beta_palette[j]

    # Plot all coefficients
    pdf( file = paste0( outdir, out_name, "_beta_coef_PC",j,".pdf"),
         width = length(x_names) + 1, height = 5)
    par(mar = c(10, 4, 4, 2) + 0.1)

    plot( 1:length(x_names), as.numeric(all_coef), xlab = " ", ylab = "Beta",
          main = paste0("Beta coefficients of possible inputs for best emulator of PC", j),
          xaxt = "n", pch = 21, cex = 1.7, col = sym_fill, bg = sym_fill)
    axis(side = 1, at = 1:length(x_names), labels = x_names, las = 2)
    abline(h=0, col = grey(0.5,0.8))
    dev.off()

    # Return all beta coefficients for this PC
    all_coef

  }) # PC loop

  # Print and plot
  cat("Table of all beta values:\n\n", file = emu_log_file, append = TRUE)
  names(beta) <- paste0("PC",1:r)
  cat("PC", x_names, "\n\n", file = emu_log_file, append = TRUE)
  for (ll in 1:length(beta)) {
    cat(names(beta)[ll], " ", paste(beta[[ll]], collapse = " "), "\n", file = emu_log_file, append = TRUE)
  }
  cat("\n\n", file = emu_log_file, append = TRUE)

  # Plot betas for all PCs (reverse order so temp at top)
  pdf( file = paste0( outdir, out_name, "_beta_coef_ALL.pdf"),
       height = length(x_names) + 1, width = 5)
  par(mar = c(5, 10, 4, 2))
  xmax <- max( abs(range(beta)) )
  plot( as.numeric(beta[[1]]), 1:length(x_names), xlim = xmax * c(-2,2),
        type = "n", yaxt = "n",
        xlab = "Beta", ylab = " ", main = "Beta coefficients: all PCs")
  axis(side = 2, at = 1:length(x_names), labels = rev(x_names), las = 2) # reversed
  abline(v=0, col = grey(0.5,0.2))

  # For each PC
  legy <- 1 # length(x_names) - r * 0.025*length(x_names)

  for (j in (r):1) {

    # Reverse to put GSAT at top
    to_plot <- rev(as.numeric(beta[[j]]))
    sym_fill <- rep(NA, length(x_names))
    sym_fill[ abs( to_plot ) > coef_tol ] <- beta_palette[j]
    points( to_plot, 1:length(x_names), pch = 21, cex = 1.7,
            col = beta_palette[j], bg = sym_fill )
    points( -1.2*xmax, legy, pch = 21, cex = 1.1,
            col = beta_palette[j], bg = beta_palette[j] )
    text( -1.2*xmax, legy, pos = 2, paste0("PC", j))
    legy <- legy + 0.025*length(x_names)

  }
  dev.off()

  # Return terms with beta > tolerance
  keep_inputs_pc <- lapply(beta, function(bb) {
    x_names[ abs(bb) > coef_tol ]

  })

  # Output terms in alphabetical order (to match plot)
  keep_inputs <- unique(unlist(keep_inputs_pc))

  n_drop <- length(x_names) - length(keep_inputs)

  if ( n_drop > 0 ) {

    cat("\nEmulator dropped these", n_drop, "inert inputs:\n", file = emu_log_file, append = TRUE)
    cat( sort(setdiff(x_names, keep_inputs)), "\n", file = emu_log_file, append = TRUE)

    cat("\nBefore:\n", file = emu_log_file, append = TRUE)
    cat(colnames(designX),"\n", file = emu_log_file, append = TRUE)

    # DROP INPUTS FROM DESIGN
    designX <- designX[ , colnames(designX) %in% keep_inputs ]

    cat("\nAfter:\n", file = emu_log_file, append = TRUE)
    cat(colnames(designX),"\n", file = emu_log_file, append = TRUE)

  }

  if ( emulator_type == "statGP") {

    # Get all inputs for trends in RobustGaSP
    trendX <- designX

    # Drop factors (dummy variable columns)
    if ( include_factors) {

      if (temp_input == "mean") {

        # Drop dummy variables from GP (design); they are still in the trends
        cat("\nmake_emu build: dropping factors from GP design:\n", file = emu_log_file, append = TRUE)
        cat(paste(c(ice_dummy_list, "\n"), collapse = " "), file = emu_log_file, append = TRUE)

        designX <- designX[ , ! colnames(designX) %in% ice_dummy_list ]

        cat("\nmake_emu build: keeping only the continuous inputs in GP design:\n", file = emu_log_file, append = TRUE)
        cat(paste(c(colnames(designX), "\n"), collapse = " "), "\n", file = emu_log_file, append = TRUE)

      }
    }
  }

  # Double-check for NAs
  stopifnot(! anyNA(designX) )
  stopifnot(! anyNA(trendX) )

  # Train emulator -----------------------------------------------------------------------

  # Emulator model for each principal component
  EMU <- lapply(1L:r, function(j) {

    if (emulator_type == "statGP") {

      # Double-check for NAs
      stopifnot(! anyNA(U[, j]) )

      emu_pc <- RobustGaSP::rgasp(design = designX, response = U[, j], trend = cbind(1, trendX),
                                  nugget.est = TRUE , lower_bound = lower_bound,
                                  kernel_type = kernel, alpha = rep(alpha, dim(as.matrix(designX))[2]))
    }

    if (emulator_type == "deepgp") {
      emu_pc <- deepgp::fit_one_layer( designX, U[, j], cov = emulator_covar, nmcmc = N_mcmc )
    }

    if (emulator_type == "dgpsi") {

      if (is_build) {

        # GP: 10x default initial nugget value; squared-exp
        emu_pc <- dgpsi::gp( designX, U[, j], name = emulator_covar, nugget_est = TRUE, nugget = 0.1)

        # DGP
        # emu_pc <- dgpsi::dgp( designX, U[, j], name = emulator_covar, nugget_est = TRUE, nugget = 0.1)

        # xxx Commented out because predict stage not currently working
        # dgpsi is based on Python so have to do things differently
        # First serialize the dgpsi emulator object
        #emu_pc_serialized <- dgpsi::serialize(emu_pc)

        # Save emulator object separately
        #emu_ser_file <- paste0(rdatadir, out_name, "_EMULATOR_PC",j,".pkl")
        #dgpsi::write(emu_pc_serialized, emu_ser_file)

      } else {

        stop("Not currently possible to run in predict mode with dgpsi")

        # A faff if coming from main.R for prediction,
        # because underlying Python means emulator objects have to be written and read separately
        #emu_ser_file <- paste0(dirname(emu_file), "/",
        #                       tools::file_path_sans_ext(basename(emu_file)),
        #                       "_PC",j,".pkl")

        # XXX Currently a failure from dgpsi::read():
        # Error in res[[paste("emulator", i, sep = "")]] : subscript out of bounds
        #if (file.exists(emu_ser_file)) {
        #  emu_ser <- dgpsi::read(emu_ser_file)
        #  emu_pc <- dgpsi::deserialize(emu_ser)
        #} else {
        #  stop(paste0("Serialized dgpsi emulator PC file not found:", emu_ser_file))
      }

      emu_pc

    } # dgpsi

    # LaGP is structured differently to other GPs:
    # Here we are just training on a random sample to estimate initial separable length scales
    # Actual training for prediction is in predict function (because train and predict are done simultaneously)
    # Adapted from laGP vignette
    if (emulator_type == "laGP") {

      # Pre-scaling is quite slow
      if (laGP_scaling) {

        # Index for random subsample of simulations
        subs <- sample(1:nrow(designX), min(1000, nrow(designX)), replace = FALSE)

        # Generates prior on length scale, based on distribution of distances between design points
        # Upper bound at 100
        d2 <- laGP::darg(list(mle = TRUE, max = 100), designX)
        cat(sprintf("\nmake_emu: length scale prior value: %.3f\n", d2$start), file = emu_log_file, append = TRUE)

        # Local approximate GP object with separable correlation structure
        # based on random sample of simulations
        # setting priors on length scales as above
        gpsepi <- laGP::newGPsep( designX[subs, ], U[subs, j],
                                  d = rep(d2$start, ncol(designX)), g = 0.1, # g = 1/1000, # might try larger initial nugget again later: 1/10
                                  dK = TRUE)

        # Maximum likelihood estimation of the length scales
        that <- laGP::mleGPsep(gpsepi, param = "d", tmin = d2$min, tmax = d2$max,
                               ab = d2$ab, maxit = 200)

        cat("\nmake_emu: length scale estimates for each input:\n", file = emu_log_file, append = TRUE)

        for (pp in 1:ncol(designX)) {
          cat(sprintf("%s: %.4f\n", colnames(designX)[pp], that$d[pp]), file = emu_log_file, append = TRUE)
        }

        # No need to keep the GP object
        laGP::deleteGPsep(gpsepi)

        # Just return length scales for scaling inputs when predicting, not emu object
        emu_pc <- that$d

      } else {

        #     # Testing code
        #     # if (FALSE) {
        #     #   # Use hard-coded for testing
        #     #   if (j==1) ls <- c(33.85, 0.04, 16.91, 16.34, 5.90, 7.81, 100.00)
        #     #   if (j==2) ls <- c(97.14, 0.03, 14.90, 10.70, 4.68, 5.77, 100.00)
        #     #   if (j==3) ls <- c(4.81, 0.04, 10.38, 7.73, 3.95, 4.76, 83.91)
        #     #   if (j==4) ls <- c(19.00, 0.03, 7.66, 4.63, 3.25, 3.72, 100.00)
        #     #   if (j==5) ls <- c(2.64, 0.02, 6.21, 3.58, 3.77, 3.46, 91.40)
        #     #   print(ls)
        #     #   ls
        #     # }

        # Return 1s i.e. no scaling
        emu_pc <- rep(1.0, ncol(designX))

      }

    } # laGP

    emu_pc

  }) # EMU: list of emulator models for each PC (or length scales from laGP)


  if (laGP_scaling) {

    # Quick plot of thetas (box plot for PCs xxx use scatter?)
    if ( emulator_type == "laGP") {

      pdf( file = paste0( outdir, out_name, "_lengthscales.pdf"),
           width = 9, height = 5)

      thats <- matrix(NA, nrow = r, ncol = ncol(designX))
      for (j in 1:r) thats[ j, ] <- EMU[[j]]

      boxplot( thats, main = paste0("Length scales (",r," PCs)"), xlab = "Emulator input",
               ylab = "Length scale")

      dev.off()
    }
  }

  #  sink()

  # Predict function ------------------------------------------------------------

  ## predict method returns a list

  pred_EMU <- function(designXout) {

    # Save design names
    # Need multi_sim flag for LOO which predicts one at a time
    if (nrow(designXout) > 1) {
      multi_sim <- TRUE
      design_names <- colnames(designXout)
    } else {
      multi_sim <- FALSE
      design_names <- names(designXout)
    }

    if (emulator_type == "statGP") {

      # Trends used in RobustGaSP
      trendXout <- designXout

      cat("\nmake_emu pred: requested input cols for prediction(s):\n", file = emu_log_file, append = TRUE)
      cat(paste(design_names, collapse = " "), "\n", file = emu_log_file, append = TRUE)

      # if any factors present
      if ( include_factors) {
        if (temp_input == "mean") {

          # Drop dummy variables from GP - they are still in the trends
          cat("\nmake_emu pred: keeping only the continuous inputs in GP design:\n", file = emu_log_file, append = TRUE)

          if (multi_sim) {
            designXout <- designXout[ , ! design_names %in% ice_dummy_list ]
            cat(paste(colnames(designXout), collapse = " "), "\n", file = emu_log_file, append = TRUE)
          } else {
            designXout <- designXout[ ! design_names %in% ice_dummy_list ]
            cat(paste(names(designXout), collapse = " "), "\n", file = emu_log_file, append = TRUE) # WORKS
            save_names <- names(designXout)

            # Make into 1 x ncol matrix again
            designXout <- matrix(designXout, nrow = 1)
            colnames(designXout) <- save_names

          }

        } # temp_input == "mean"
      } # if include_factors

      # Original list of inputs before lasso drops inert
      cat("\nmake emu pred: original list of inputs:\n", file = emu_log_file, append = TRUE)
      cat("GP (",ncol(designXout),"):", paste(colnames(designXout), collapse = " "),"\n", file = emu_log_file, append = TRUE)

      if (multi_sim) {
        cat("Trends (",ncol(trendXout),"):", paste(colnames(trendXout), collapse = " "),"\n", file = emu_log_file, append = TRUE)
      } else {
        cat("Trends (",ncol(trendXout),"):", paste(names(trendXout), collapse = " "),"\n", file = emu_log_file, append = TRUE)
      }

      cat("\nmake emu pred: checking for inert inputs...\n", file = emu_log_file, append = TRUE)

      # Check if any design columns to drop still
      if ( length(setdiff(colnames(designXout), keep_inputs)) == 0 ) {

        cat("No inert inputs to drop\n", file = emu_log_file, append = TRUE)

      } else {

        # Drop inert inputs
        if (multi_sim) {

          designXout <- designXout[ , colnames(designXout) %in% keep_inputs ]
          trendXout <- trendXout[ , colnames(trendXout) %in% keep_inputs ]

        } else { # 1 sim (i.e. LOO)

          save_names_all <- colnames(designXout)
          save_names <- save_names_all[ save_names_all %in% keep_inputs]
          cat(save_names_all, "\n", file = emu_log_file, append = TRUE)
          cat(save_names, "\n", file = emu_log_file, append = TRUE)

          designXout <- designXout[ colnames(designXout) %in% keep_inputs ]
          trendXout <- trendXout[ names(trendXout) %in% keep_inputs ]

          # Reformat as matrices
          designXout <- matrix(designXout, nrow = 1)
          colnames(designXout) <- save_names

          save_names <- names(trendXout)
          trendXout <- matrix(trendXout, nrow = 1)
          colnames(trendXout) <- save_names

        }
      }

      # Output active inputs for prediction
      cat("\nmake emu pred: keeping these inputs in emulator design:\n",
          file = emu_log_file, append = TRUE)
      cat("GP final (",ncol(designXout),"):", paste(colnames(designXout), collapse = " "),"\n", file = emu_log_file, append = TRUE)
      cat("Trends final (",ncol(trendXout),"):", paste(colnames(trendXout), collapse = " "),"\n\n", file = emu_log_file, append = TRUE)

      # Moved Jonty's ncol check for designXout from start of robj to here, and add for trendXout to be sure
      # Same inputs used for all PCs so can compare with PC1
      stopifnot(ncol(designXout) == EMU[[1]]@p) # @p GP inputs in PC1 emulator build

      cat("CHECK:",ncol(EMU[[1]]@X) - 1, "\n", file = emu_log_file, append = TRUE)
      #cat(EMU[[1]]@X, "\n", file = emu_log_file, append = TRUE)

      stopifnot(ncol(trendXout) == ncol(EMU[[1]]@X) - 1) # @X: Trends in PC1 emulator build (drop col of 1s)

      # Predict for set of new design points using each PC emulator in list
      EMU_pred <- lapply(EMU, function(emu) {

        RobustGaSP::predict( emu, testing_input = designXout,
                             testing_trend = cbind(1, trendXout) )[c("mean", "sd")]
      })
    } # statGP

    if (emulator_type == "deepgp") {
      EMU_pred <- lapply(EMU, function(emu) {
        deepgp::predict( emu, designXout )[c("mean", "s2")]
      })
    }

    if (emulator_type == "dgpsi") {

      # Get emulators for PCs
      EMU_pred <- lapply(EMU, function(emu) {

        # Use to predict PC
        pred <- predict(emu, designXout)
        list( mean = pred$results$mean, var = pred$results$var)

      })

    } # dgpsi

    if (emulator_type == "laGP") {

      # Loops over PCs to get length scales (if estimated)
      EMU_pred <- lapply(1L:r, function(j) {

        # LaGP builds and predicts at the same time

        # Get full ensemble and prediction designs,
        # responses, and length scales to scale inputs in both designs
        # Latter two are for j-th PC
        designX_scaled <- designX
        designXout_scaled <- designXout
        response <- U[ , j]
        scales <- sqrt(EMU[[j]]) # EMU here are theta; laGP recommends using sqrt(theta)

        # LaGP selects subsample to train with for each new prediction
        # Use prior length scale = 1 because inputs rescaled

        if (laGP_scaling) {

          # Scale each column with sqrt(length scale) returned by earlier EMU object
          for(pp in 1:ncol(designX)) {
            designX_scaled[, pp] <- designX_scaled[, pp] / scales[pp]
            designXout_scaled[, pp] <- designXout_scaled[, pp] / scales[pp]
          }

          # Build and predict from local design, using scaled inputs
          lagp_pred <- laGP::aGP(designX, response, designXout, g = laGP_nugget_prior,
                                 d = list(start = 1, max = 20),
                                 method = laGP_method)[c("mean", "var", "d", "g")]
        } else {

          # Build and predict from local design, using original inputs
          lagp_pred <- laGP::aGP(designX, response, designXout, g = laGP_nugget_prior,
                                 method = laGP_method)[c("mean", "var", "d", "g")]
        }

        lagp_pred

      })

    } # if laGP

    # Return predictions
    EMU_pred

  } # pred_EMU

  # Emulator prediction object -------------------------------------------------

  ## return a function

  # Can return mean function (currently not used), mean and sd (used for single year LOO predictions),
  # or (default) mean, sd and var
  robj <- function(designXout, forcingXout, type = c("mean", "sd", "var")) {

    # Save column names
    if (!is.null(dim(designXout))) {
      multi_sim <- TRUE
      design_names <- colnames(designXout)
    } else {
      multi_sim <- FALSE
      design_names <- names(designXout)
    }

    type <- match.arg(type)

    if (!is.matrix(designXout) && length(designXout) == n_inputs) {
      dim(designXout) <- c(1L, n_inputs)
    } else {
      # moved ncol check inside pred_emu because requested design might include extra (inert) inputs
      stopifnot(is.matrix(designXout)) # ncol(designXout) == n_inputs)
    }
    n_predict <- nrow(designXout) # renamed m_out n_predict

    # Put names back xxx check when dropped
    if (multi_sim) { colnames(designXout) <- design_names
    } else names(designXout) <- design_names

    # Make predictions for r PCs
    pplist <- pred_EMU(designXout) # r-list

    # Output laGP estimated length scales and nuggets, e.g. for testing
    if (emulator_type == "laGP") {
      darg <- sapply(pplist, "[[", "d") # n_predict x r
      print(darg)
      garg <- sapply(pplist, "[[", "g") # n_predict x r
      print(garg)
    }

    ## compute the time series (n_times time slices) of mean values
    # for each of the n_predict design points
    mu <- sapply(pplist, "[[", "mean") # n_predict x r
    dim(mu) <- c(n_predict, r)
    mx <- sweep(mu %*% Vt, 2L, cc, "+") # n_predict x n_times

    # Can choose to return only the mean
    if (type == "mean")
      return(list(mean = mx))

    ## compute the sd from the PCs similarly: note most packages output var not sd
    if (emulator_type == "statGP") sdu <- sapply(pplist, "[[", "sd") # n_predict x r
    if (emulator_type == "deepgp") sdu <- sqrt(sapply(pplist, "[[", "s2"))
    if (emulator_type == "dgpsi") sdu <- sqrt(sapply(pplist, "[[", "var"))
    if (emulator_type == "laGP") sdu <- sqrt(sapply(pplist, "[[", "var"))

    dim(sdu) <- c(n_predict, r)
    sdx <- lapply(1L:n_predict, function(i) {
      sqrt(colSums((sdu[i, ] * Vt)^2)) # n_times vector
    })
    sdx <- do.call("rbind", sdx) #  n_predict x n_times

    # Can choose to return only the s.d.
    if (type == "sd") return(list(mean = mx, sd = sdx))

    ## compute the variance - i.e. covariances between the n_times time slices
    # for each design point
    Sx <- lapply(1L:n_predict, function(i) {
      as.vector(crossprod(sdu[i, ] * Vt)) # n_times*n_times vector
    })
    Sx <- do.call("cbind", Sx) # n_times*n_times x n_predict
    dim(Sx) <- c(n_times, n_times, n_predict)
    Sx <- aperm(Sx, c(3, 1, 2))  # n_predict x n_times*n_times

    # Default is to return mean, sd and var
    return(list(mean = mx, sd = sdx, var = Sx, inputs = keep_inputs))
  }

  ## class and return

  cat("\nmake_emu: end of emulator build\n",file = emu_log_file, append = TRUE)
  cat("_____________________________________\n",file = emu_log_file, append = TRUE)

  structure(robj, class = "emu")

}


# END MAKE_EMU()
