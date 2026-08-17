#' match_gcms: match ice simulations to climate forcing simulations
#'
#' @description
#' Look up climate simulation for each ice simulation by SSP and GCM, check
#' it exists, and calculate climate change for emulator input.
#'
#' @returns `match_gcms()` returns requested temperature change(s) for each
#' simulation.
#'
#' @export

match_gcms <- function(ice_data, temps_dataset, mean_impute = FALSE) {

  # Match climate and ice sims ---------------------------------------------------------------------

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("match_gcms: matching ice model simulations with forcing simulations\n\n",
      file = logfile_build, append = TRUE)

  # Write temps into ice_data length matrix, not forcing length, to use in ice_design
  temps <- ice_data[ , c("scenario", "GCM")]

  # For each GSAT change (column), we will retrieve val or use mean of other sims for scenario
  # warning if entirely replacing
  for (tt in 1:length(temps_list)) {

    temps <- cbind(temps, NA)
    colnames(temps)[ tt + 2 ] <- paste0("GSAT_", temps_list[tt])

    # Fill column by row
    temps[ , tt + 2] <- unlist(apply(ice_data, 1, function(x) {

      # Get temps values for row
      gcm_vals <- temps_dataset[ temps_dataset$GCM == x["GCM"]
                                 & temps_dataset$scenario == x["scenario"],
                                 temps_list_names, drop = FALSE ]

      # Check if GCM totally missing and also if present but all temps missing
      # The latter suggests the baseline had missing data so we want to
      # stop and tell the user
      gcm_absent <- nrow(gcm_vals) != 1L
      gcm_row_all_na <- nrow(gcm_vals) == 1L &&
        !any(is.finite(unlist(gcm_vals)))

      # Retrieve value for this timeslice (column) if GCM forcing was found
      temp_row <- if (!gcm_absent) gcm_vals[[tt]] else NA

      # Fail on first timeslice if all GSAT values are missing
      if (gcm_row_all_na && tt == 1L) {
        stop("All GSAT timeslices NA, so baseline ", temps_baseline_start, "-", temps_baseline_end,
             " likely has missing data for matched forcing ", x["scenario"], " ", x["GCM"],
             " . Please try a later baseline, i.e. with non-missing years in the climate forcing CSV.",
             call. = FALSE)
      }

      # If this GSAT timeslice is missing
      if (length(temp_row) == 0L || is.na(temp_row)) {

        # If requested impute and GCM row is not 'present-but-all-missing'
        if (mean_impute && !gcm_row_all_na) {

          # Visibly warn if the GCM row is wholly absent (only the first time, i.e. tt == 1).
          # Repeats for all ice sims with this GCM forcing
          if (gcm_absent && tt == 1L) {
            warning("match_gcms: missing GCM forcing is being replaced by ensemble mean for scenario - do you want this?")
            cat("match_gcms: missing GCM forcing is being replaced by ensemble mean:",
                x["scenario"], x["GCM"], "\n",
                file = logfile_build, append = TRUE)
          }

          # Impute with ensemble mean either way
          temp_row <- temps_dataset[
            temps_dataset$GCM == "ensemble_mean" &
              temps_dataset$scenario == x["scenario"], tt + 2]

          # If not requested impute, or present-but-all-missing, then GSAT stays missing
         } else temp_row <- NA
      }

      if (length(temp_row) == 0) temp_row <- NA
      temp_row

    }))


  }

  # Useful to know what we have: rows with data in final column
  df_found <- unique( temps[ ! is.na(temps[ , 2 + length(temps_list)]),] )

  if (mean_impute) {
    cat(paste("Found or imputed", dim(df_found)[1], "complete forcings for", length(unique(df_found[, "scenario"])), "scenarios for",
              dim(temps[ ! is.na(temps[ , 2 + length(temps_list)]),])[1], "simulations:\n\n"),
        file = logfile_build, append = TRUE)
  } else {
    cat(paste("Found", dim(df_found)[1], "complete forcings for", length(unique(df_found[, "scenario"])), "scenarios for",
              dim(temps[ ! is.na(temps[ , 2 + length(temps_list)]),])[1], "simulations:\n\n"),
        file = logfile_build, append = TRUE)
  }

  # Sort alphabetically by scenario to print
  ms <- df_found[ sort(df_found[,"scenario"], index.return = TRUE)$ix, ]
  for( mm in 1:dim(ms)[1]) {
    cat( unlist(ms[mm, c("scenario", "GCM")]), "\n", file = logfile_build, append = TRUE)
  }

  # And what we don't: missing final column
  df_miss <- unique( temps[is.na(temps[ , 2 + length(temps_list)]),] )
  if (dim(df_miss)[1] > 0) {
    cat(paste("\nCould not find part/all of", dim(df_miss)[1],"forcings for", dim(temps[is.na(temps[ , 2 + length(temps_list)]),])[1],
              "simulations (will be skipped, or forcings may be reconstructed below (e.g. fixed climate, or imputed):\n\n"), file = logfile_build, append = TRUE)
    cat(paste(df_miss[ ,"scenario"], df_miss[ ,"GCM"], "\n"), "\n", file = logfile_build, append = TRUE)
  }

  cat(paste("\nmatch_gcms: found", dim(temps[ ! is.na(temps[ , 2 + length(temps_list)]),])[1], "of", dim(temps)[1],
            "forcing simulations\n"), file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  # nrows(ice_data) x {scenario, GCM, temps}
  return(temps)

}
