#' check_design: check design matrix before sending to make_emu for emulation
#'
#' @description
#' Test rank, conditioning, and other info and write to log file
#' Written with some input from Cursor AI.
#'
#' @param designX Dataset design matrix
#'
#' @returns `check_design()` returns 1 if any potential problems found; 0 otherwise.
#'

check_design <- function(designX) {

  cat("\ncheck_design: running tests on design matrix for emulation\n", file = emu_log_file, append = TRUE)

  stopifnot(is.matrix(designX))

  # Thresholds for kappa, minimum variance of continuous inputs,
  # minimum fraction of non-reference factor levels, and GSAT correlation
  k_thresh <- 1e8
  var_thresh <- 1e-6
  frac_thresh <- 0.01

  # Start with OK, and add 1 for each potential problems found
  is_design_OK <- 0

  # Colinearity check ----------------------------------------------------------
  # Cursor AI

  # Check rank and condition of GSAT_only columns (to ensure drop_temps() did
  # enough to avoid collinearity), then full design

  for (test_matrix in c("GSAT columns of", "full design")) {

    if (test_matrix == "GSAT columns of" ) testX <- designX[ , colnames(designX) %in% temps_list_names ]
    if (test_matrix == "full design" ) testX <- designX

    cat("\ncheck_design: checking",test_matrix,"matrix\n", file = emu_log_file, append = TRUE)

    # Check ensemble is not rank deficient
    # (e.g. in GIS 2300 ensemble, resolution and init_yrs are confounded)

    cat("\nChecking rank...\n", file = emu_log_file, append = TRUE)
    cat("\nRank:", qr(testX)$rank, "\n", file = emu_log_file, append = TRUE)
    cat("Number of columns:", ncol(testX),"\n", file = emu_log_file, append = TRUE)
    if (qr(testX)$rank == ncol(testX)) cat("Matrix is full rank\n", file = emu_log_file, append = TRUE)

    # Compute rank deficiency
    if (qr(testX)$rank < ncol(testX)) {
      cat(sprintf("\nNOTE: ensemble is rank deficient: rank %i is less
                than number of columns %i\n",
                  qr(testX)$rank, ncol(testX)),
          file = emu_log_file, append = TRUE)

      # Identify redundant (aliased) columns from QR pivot
      qr_obj <- qr(testX)
      dep_idx <- qr_obj$pivot[(qr_obj$rank + 1):ncol(testX)]
      confounded <- colnames(testX)[dep_idx]

      # Fallback names if missing
      if (is.null(confounded)) confounded <- paste0("V", dep_idx)

      cat("\nAliased/redundant columns (drop one or more):\n",
          file = emu_log_file, append = TRUE)
      cat(paste(confounded, collapse = ", "), "\n", file = emu_log_file, append = TRUE)

      # Print alias equations
      tmp_df <- as.data.frame(testX)
      tmp_df$.__y__ <- rnorm(nrow(tmp_df))
      ali <- alias(stats::lm(.__y__ ~ ., data = tmp_df))

      cat("\nAlias structure (Complete):\n", file = emu_log_file, append = TRUE)
      capture.output(print(ali$Complete), file = emu_log_file, append = TRUE)

      # Non-zero return because rank deficient
      is_design_OK <- is_design_OK + 1
      cat("\ncheck_design: ** Warning! Design matrix is rank deficient. Consider dropping inputs ** \n", file = emu_log_file, append = TRUE)
      warning("Design matrix is rank deficient: consider dropping inputs")

    }

    # Further checks ------------------------------------------------
    # Conditioning: Cursor AI

    # 1. Condition number of the ensemble design
    # If k > 1e12–1e15, that’s problematic
    cat("\nChecking condition...\n", file = emu_log_file, append = TRUE)
    k <- kappa(testX, exact = TRUE)
    cat("\nCondition number:", k, "\n", file = emu_log_file, append = TRUE)
    cat("Threshold is", k_thresh, "\n", file = emu_log_file, append = TRUE)

    # Non-zero return if ill-conditioned
    if ( k > k_thresh ) {
      is_design_OK <- is_design_OK + 1
      cat("\ncheck_design: ** Warning! Design matrix is ill-conditioned ** \n", file = emu_log_file, append = TRUE)
      warning("Design matrix is ill-conditioned: k > k_thresh")
    }

  } # End of duplicate tests on GSAT-only columns and full design

  cat("\ncheck_design: further checks on full design matrix\n", file = emu_log_file, append = TRUE)

  # 2. Check for (near) zero-variance columns, i.e. (nearly) constant values
  # Zero or tiny variance columns can break QR/kappa
  cat("\nChecking for zero/near-zero variance of continuous inputs....\n", file = emu_log_file, append = TRUE)
  cat("\nVariances:\n", file = emu_log_file, append = TRUE)
  designX_cont <- designX[ , ! colnames(designX) %in% ice_dummy_list ]
  test_var <- apply(designX_cont, 2, function(z) v <- var(z))
  for( pp in 1:length(colnames(designX_cont))) {
    cat(paste(colnames(designX_cont)[pp],test_var[pp],"\n"), file = emu_log_file, append = TRUE)
  }
  cat("\nMinimum variance =", min(test_var),"\n", file = emu_log_file, append = TRUE)
  cat("Threshold is",var_thresh,"\n", file = emu_log_file, append = TRUE)

  # Non-zero return if any tiny variance
  if ( abs(min(test_var)) < var_thresh ) {
    is_design_OK <- is_design_OK + 1
    cat("\ncheck_design: ** Warning! Design matrix has low variance input(s) ** \n", file = emu_log_file, append = TRUE)
    warning("Design matrix has at least one input with very low variance (< var_thresh)")
  }

  # 3. Similar check for dummy variables (non-reference levels only)
  if (include_factors) {

    cat("\nChecking for rare non-reference levels of factor inputs...\n\n", file = emu_log_file, append = TRUE)
    designX_fac <- designX[ , colnames(designX) %in% ice_dummy_list ]

    # Fraction that are the named level
    test_frac <- apply(designX_fac, 2, function(z) mean(z))
    for( pp in 1:length(colnames(designX_fac))) {
      cat(paste(colnames(designX_fac)[pp],test_frac[pp],"\n"), file = emu_log_file, append = TRUE)
    }
    cat("\nMinimum fraction =", round(100.0*min(test_frac),1),"%\n", file = emu_log_file, append = TRUE)
    cat("Threshold is", round(100.0*frac_thresh, 1),"%\n", file = emu_log_file, append = TRUE)

    # Option to stop completely if rare levels
    if ( min(test_frac) < frac_thresh) {
      is_design_OK <- is_design_OK + 1
      cat("\ncheck_design: ** Warning! Design matrix has very low sampling of factor level(s) ** \n",
          file = emu_log_file, append = TRUE)
      warning("Design matrix has at least one non-reference factor level with very low fraction (< frac thresh)")
    }
  }

  cat("\nOther info (these should not be too small):\n", file = emu_log_file, append = TRUE)

  # Very large magnitudes can overflow
  # 4. Singular values (alternative to kappa)
  s <- svd(scale(designX))$d

  # Very small min(s) or tiny ratio suggests ill-conditioning
  cat("\nSmallest singular value :", min(s), "\n", file = emu_log_file, append = TRUE)
  cat("Ratio of smallest/largest singular values:", min(s)/max(s), "\n", file = emu_log_file, append = TRUE)

  if (is_design_OK == 0) { cat("\ncheck_design: passed all matrix checks\n", file = emu_log_file, append = TRUE)
  } else cat("\ncheck_design: failed",is_design_OK,"matrix checks\n", file = emu_log_file, append = TRUE)

  # Return 0 or 1
  is_design_OK

}
