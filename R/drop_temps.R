#' drop_temps: check for highly correlated GSAT timeslices to drop
#'
#' @description
#' Test GSAT timeslice correlation and return list to drop.
#' Written with some input from Cursor AI.
#'
#' @param designX Dataset design matrix
#'
#' @returns `drop_temps()` returns subset of temp_list_names to drop, or NA.
#'

drop_temps <- function(designX) {

  drop_temp_list <- NA
  n_high_corr <- NA

  # Correlation threshold
  cor_thresh <- 0.80
  cat(sprintf("\nThreshold (Kendall's tau) = %.2f\n", cor_thresh), file = emu_log_file, append = TRUE)

  # Print correlations for all initial timeslices
  cat("\ndrop_temps: Checking correlations of GSAT timeslice columns...\n\n", file = emu_log_file, append = TRUE)
  corr_temps <- cor(designX[, colnames(designX) %in% temps_list_names, drop = FALSE], method = "kendall")

  cat("\t\t", paste(colnames(designX)[ colnames(designX) %in% temps_list_names], collapse = "\t"), "\n", file = emu_log_file, append = TRUE)
  write.table(corr_temps, sep = "\t", quote = FALSE, col.names = FALSE, file = emu_log_file, append = TRUE)

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
        corr_mean[1] <- mean(corr_temps[ corr_pairs[ max_corr, 1], other_cols ])

        # Do same for second timeslice of pair
        other_rows <- 1:nrow(corr_temps)
        other_rows <- other_rows[ ! other_rows %in% corr_pairs[ max_corr, ] ]
        corr_mean[2] <- mean(corr_temps[ other_rows, corr_pairs[ max_corr, 2] ])

        # Worst (most correlated with other timeslices)
        to_drop <- names(corr_mean)[which.max(corr_mean)]

        cat(sprintf("check_design: Dropping highest correlation GSAT: %s\n", to_drop),
            file = emu_log_file, append = TRUE)

      }

      # Add timeslice to drop list
      drop_temp_list <- c(drop_temp_list, to_drop)

      # Drop from design and go back to start of while to recalculate correlations
      designX <- designX[ , ! colnames(designX) %in% drop_temp_list, drop = FALSE ]

    } else {
      cat("\nNo columns have correlation |tau| >",cor_thresh,"\n", file = emu_log_file, append = TRUE)
      break
    }

    # Drop initial NA if some timeslices to drop
    if ( length(drop_temp_list) > 1 && is.na(drop_temp_list[1])) drop_temp_list <- drop_temp_list[-1]

  } # while

  cat("\ndrop_temps: Final correlations of GSAT timeslice columns:\n\n", file = emu_log_file, append = TRUE)
  corr_temps <- cor(designX[, colnames(designX) %in% temps_list_names, drop = FALSE], method = "kendall")

  cat("\t\t", paste(colnames(designX)[ colnames(designX) %in% temps_list_names], collapse = "\t"), "\n", file = emu_log_file, append = TRUE)
  write.table(corr_temps, sep = "\t", quote = FALSE, col.names = FALSE, file = emu_log_file, append = TRUE)

  cat("\nKeeping timeslices:",paste(temps_list_names[ ! temps_list_names %in% drop_temp_list ], collapse=" "),
      "\n", file = emu_log_file, append = TRUE)

  # Return NA or list of temp columns
  drop_temp_list

}
