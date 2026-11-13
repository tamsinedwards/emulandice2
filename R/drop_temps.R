#' drop_temps: choose GSAT timeslices to drop
#'
#' @description
#' Test GSAT time periods with PCD or correlation and return list to drop.
#' Written with some input from Cursor AI.
#'
#' @param designX Dataset design matrix
#'
#' @returns `drop_temps()` returns subset of temp_list_names to drop, or NA.
#'

# Applied to temperature mean columns of ensemble design
# TODO: also use CMIP6+7 FaIR

# Select temp inputs that explain most of the variance
# Adapted from code snippet in
# "Computer Experiments: A Practical Guide to Simulating Complex Systems"
# by Jonty Rougier; Oct 2026 manuscript
pchol <- function(Y) {

  # Check inputs
  stopifnot(is.matrix(Y), is.numeric(Y), !anyNA(Y))
  m <- ncol(Y)
  cnms <- colnames(Y)

  # Sweep out the column means
  Y <- sweep(Y, 2L, colMeans(Y), "-") # column centre

  # Pivoted Cholesky decomposition
  suppressWarnings(
    G <- t(base::chol(crossprod(Y), pivot = TRUE)) )
  r <- attr(G, "rank")
  pivot <- attr(G, "pivot")[seq_len(r)]

  d <- vapply(seq_len(r), \(i) {
    sum(G[i:m, i]^2)
  }, FUN.VALUE = double(1))

  scree <- cumsum(d) / sum(d)

  robj <- data.frame(pivot = pivot, scree = scree, d = d)
  if (!is.null(cnms)) robj$label <- cnms[robj$pivot]

  robj

}

drop_temps <- function(designX) {

  # Keep list to return
  drop_temp_list <- NA

  if (drop_temps_method == "pcd") {

    # TODO: move scree threshold to emulator_build.R or YAML
    pcd_thresh <- 0.9999

    cat("\ndrop_temps: selecting GSAT timeslices(s) with PCD using threshold", 100.0*pcd_thresh,
        "%%\n", file = emu_log_file, append = TRUE)

    # Temp mean columns of ensemble design
    designX_temps <- designX[, colnames(designX) %in% temps_list_names, drop = FALSE]

    # Pivoted Chol decomposition
    pcd <- pchol(designX_temps)

    # Keep components that explain 99.X% of variance
    # Use first if explains
    if (length(pcd$label[ pcd$scree <= pcd_thresh ]) == 0) {
      pcd_keep <- 1
    } else {
      pcd_keep <- pcd$scree <= pcd_thresh
    }

    cat("\ndrop_temps: selecting GSAT timeslices(s) with PCD using threshold", 100.0*pcd_thresh,
        "%%\n", file = emu_log_file, append = TRUE)

    cat("drop_temps: using ", pcd$label[1],
                "GSAT periods explaining",100.0*pcd$scree[length(pcd_keep)],
              "%% of variance\n", file = emu_log_file, append = TRUE)

    cat(paste(pcd$label[ pcd_keep ], collapse = " "), "\n", file = emu_log_file, append = TRUE)
    cat(paste(pcd$scree[ pcd_keep ], collapse = " "), "\n", file = emu_log_file, append = TRUE)

    # Plot (with selected components in red)
    pdf( file = paste0( plotdir, out_name, "_PCD.pdf"),
         width = 9, height = 5)
    plot( seq_len(length(pcd$scree)), pcd$scree, main = "Scree plot for PCD of GSAT means",
          type = "p", pch = 20, xlab = "Number of components",
          ylab = "% of variance explained")
    points( seq_len(length(pcd_keep[pcd_keep == TRUE])), pcd$scree[pcd_keep], pch = 20, col = "red")
    abline(h=pcd_thresh)
    dev.off()

    # List of columns to drop
    drop_temp_list <- colnames(designX_temps)[ ! colnames(designX_temps) %in% pcd$label[pcd_keep] ]

  }

  if (drop_temps_method == "corr") {

    n_high_corr <- NA

    # Print correlations for all initial timeslices
    cat("\ndrop_temps: Checking correlations of GSAT timeslice columns...\n\n", file = emu_log_file, append = TRUE)
    corr_temps <- cor(designX[, colnames(designX) %in% temps_list_names, drop = FALSE], method = "kendall")

    cat("\t\t", paste(colnames(designX)[ colnames(designX) %in% temps_list_names], collapse = "\t"), "\n", file = emu_log_file, append = TRUE)
    write.table(corr_temps, sep = "\t", quote = FALSE, col.names = FALSE, file = emu_log_file, append = TRUE)

    # Warn and drop if any GSAT timeslice(s) zero-variance
    # This is rare but can happen e.g. if only selecting Kori/PISM with a baseline before 1980 because they all use NorESM1-M
    temp_cols <- colnames(designX)[colnames(designX) %in% temps_list_names]
    temp_const <- temp_cols[vapply(temp_cols, function(nm) isTRUE(sd(designX[, nm], na.rm = TRUE) == 0), logical(1))]

    if (length(temp_const)) {
      warning("drop_temps: removing zero-variance GSAT column(s): ", paste(temp_const, collapse = ", "))
      cat("\ndrop_temps: removing zero-variance GSAT column(s): ", paste(temp_const, collapse = ", "),"\n", file = emu_log_file, append = TRUE)

      # Drop from design inside this function, and add to drop list to return
      designX <- designX[ , ! colnames(designX) %in% temp_const, drop = FALSE ]
      drop_temp_list <- c(drop_temp_list, temp_const)

    }
    cat("\ndrop_temps: Iterating to drop highest correlation columns until all below threshold |tau| =< ",cor_thresh,"\n\n", file = emu_log_file, append = TRUE)

    while( is.na(n_high_corr) || (! is.na(n_high_corr) && n_high_corr > 0) ) {

      #cat("\ndrop_temps: Checking correlations of GSAT timeslice columns...\n\n", file = emu_log_file, append = TRUE)
      corr_temps <- cor(designX[, colnames(designX) %in% temps_list_names, drop = FALSE], method = "kendall")

      #cat("\t\t", paste(colnames(designX)[ colnames(designX) %in% temps_list_names], collapse = "\t"), "\n", file = emu_log_file, append = TRUE)
      #write.table(corr_temps, sep = "\t", quote = FALSE, col.names = FALSE, file = emu_log_file, append = TRUE)

      # Get pair-wise correlations that are above threshold
      # Each row is a highly correlated pair of timeslices: the first column is the row number, second is the col number
      # So 3,4 would be high correlation between the third and fourth time slices
      corr_pairs <- which(abs(corr_temps) > cor_thresh & upper.tri(corr_temps), arr.ind = TRUE)

      # Set from NA to integer
      n_high_corr <- nrow(corr_pairs)

      # If find any highly correlated GSAT timeslices
      if (n_high_corr > 0) {

        # Which row of corr_pairs has indices of maximum pair-wise correlation?
        max_corr <- which.max(apply( corr_pairs, 1, function(x) corr_temps[x[1], x[2]] ))

        # Store mean correlations with other timeslices
        corr_mean <- rep(NA,2)
        names(corr_mean) <- c(rownames(corr_temps)[corr_pairs[max_corr, 1]],
                              colnames(corr_temps)[corr_pairs[max_corr, 2]])

        # First of pair
        other_cols <- 1:ncol(corr_temps)
        other_cols <- other_cols[ ! other_cols %in% corr_pairs[ max_corr, ] ]

        # If only two timeslices
        if (length(other_cols) == 0) {

          # Get years and keep later one
          yy_list <- as.numeric(sapply(strsplit(names(corr_mean), split = "_"), "[", 2))
          to_drop <- names(corr_mean)[which.min(yy_list)] #names(corr_mean)[2]

          cat(sprintf("check_design: Dropping earliest of high correlation GSAT pair: %s\n", to_drop),
              file = emu_log_file, append = TRUE)

        } else { # If more timeslices

          # Calculate mean correlation of first timeslice in pair with others
          corr_mean[1] <- mean(corr_temps[ corr_pairs[ max_corr, 1], other_cols ], na.rm = TRUE)

          # Do same for second timeslice of pair
          other_rows <- 1:nrow(corr_temps)
          other_rows <- other_rows[ ! other_rows %in% corr_pairs[ max_corr, ] ]
          corr_mean[2] <- mean(corr_temps[ other_rows, corr_pairs[ max_corr, 2] ], na.rm = TRUE)

          # Worst (most correlated with other timeslices)
          to_drop <- names(corr_mean)[which.max(corr_mean)]

          cat(sprintf("check_design: Dropping highest correlation GSAT: %s\n", to_drop),
              file = emu_log_file, append = TRUE)

        }

        # Add timeslice to drop list
        drop_temp_list <- c(drop_temp_list, to_drop)

        # Drop from design and go back to start of while to recalculate correlations
        # (internal use only: designX is modified from drop_temp_list later)
        designX <- designX[ , ! colnames(designX) %in% drop_temp_list, drop = FALSE ]

      } else {
        cat("\nNo columns have correlation |tau| >",cor_thresh,"\n", file = emu_log_file, append = TRUE)
        break
      }

    } # while

    # Drop initial NA if some timeslices to drop
    if ( length(drop_temp_list) > 1 && is.na(drop_temp_list[1])) drop_temp_list <- drop_temp_list[-1]

    cat("\ndrop_temps: Final correlations of GSAT timeslice columns:\n\n", file = emu_log_file, append = TRUE)
    corr_temps <- cor(designX[, colnames(designX) %in% temps_list_names, drop = FALSE], method = "kendall")

    cat("\t\t", paste(colnames(designX)[ colnames(designX) %in% temps_list_names], collapse = "\t"), "\n", file = emu_log_file, append = TRUE)
    write.table(corr_temps, sep = "\t", quote = FALSE, col.names = FALSE, file = emu_log_file, append = TRUE)


  } # correlation method

  cat("\nKeeping timeslices:", paste(temps_list_names[ ! temps_list_names %in% drop_temp_list ], collapse=" "),
      "\n", file = emu_log_file, append = TRUE)

  # Return NA or list of temp columns
  drop_temp_list

}
