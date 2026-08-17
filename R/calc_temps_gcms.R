#' calc_temps_gcms: calculate climate change metric(s) for emulator inputs
#'
#' @description
#' Calculate climate change metric(s) for emulator input from GCM forcings
#'
#' @param climate_dataset Climate data matrix object
#' @param mean_impute TRUE/FALSE to calculate ensemble mean of SSP to use as
#' forcings for simulations extended with impute_sims = "extend" (xxx check this)
#'
#' @returns `calc_temps_gcms()` returns requested temperature change(s) for all
#' simulations in climate forcing file.
#'
#' @export

calc_temps_gcms <- function(climate_dataset, mean_impute = FALSE) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat( "calc_temps_gcms: calculating temperature change(s) for all forcings in dataset\n\n", file = logfile_build, append = TRUE )
  if (mean_impute) cat( "Imputing missing GCMs with mean of others for scenario\n\n",
                        file = logfile_build, append = TRUE )

  # Drop scenario, vals before calculating just to save indexes (re-adds after)
  climate_vals <- climate_dataset[, -c(1, 2), drop = FALSE]

  # Calculate GSAT anomalies: snippet refactor from Cursor
  # Calls helper function gsat_anom_row from calc_temps_functions.R
  climate_anom <- t(apply(climate_vals, 1, gsat_anom_row,
                          end_years = temps_list,
                          baseline_end = temps_baseline_end,
                          n_yrs = N_temp_yrs))
  climate_anom <- matrix(climate_anom, ncol = length(temps_list),
                         dimnames = list(NULL, temps_list_names))

  # Add scenario and GCM columns back
  climate_anom <- cbind(climate_dataset[, 1:2, drop = FALSE], climate_anom)

  # Add extra rows for ensemble mean if imputing simulations
  if (mean_impute) {

    # Checking final GSAT timeslice
    tt_last <- temps_list_names[length(temps_list_names)]

    # For each scenario in dataset
    for ( scen in unique(climate_dataset[ , "scenario" ])) {

      # Get rows with non-missing final value for scenario
      scen_ens <- climate_anom[ climate_anom$scenario == scen &
                                  ! is.na(climate_anom[, tt_last ]), ]

      # If some GCMs exist for this scenario
      if ( dim(scen_ens)[1] > 0) {

        cat(paste0("\n\nFound last timeslice ", tt_last, " for ",dim(scen_ens)[1],
                   " GCMs to compute ensemble_mean for ", scen, ":\n" ),
            file = logfile_build, append = TRUE)

        if ( dim(scen_ens)[1] == 1 ) {

          # If one GCM, take value directly
          scen_ens_impute <- scen_ens[ , temps_list_names ]

        } else {

          # Average each column (1 for each temp_list value) and add to end
          if (length(temps_list) == 1) { scen_ens_impute <- mean( as.numeric(scen_ens[ , temps_list_names]) )
          } else {
            scen_ens_impute <- apply( scen_ens[ , temps_list_names], 2, function(x) {
              mean(as.numeric(x)) })
          }
        }

        cat(paste("GCM", "\t", paste(temps_list_names, collapse = "\t"), "\n"),
            file = logfile_build, append = TRUE)

        for (cc in 1:dim(scen_ens)[1]) {
          cat(paste(scen_ens[ cc, "GCM"],  "\t", paste(scen_ens[cc, temps_list_names], collapse = "\t"), "\n"),
            file = logfile_build, append = TRUE)
        }

        cat(paste("ensemble_mean",  "\t", paste(scen_ens_impute, collapse = "\t"), "\n", collapse = " "),
            file = logfile_build, append = TRUE)

        # Append as a data.frame row (not c(...)): c() would coerce all GSAT
        # columns to character, and match_gcms is.finite() would then treat
        # every GCM as all-NA and stop on a false "baseline missing" error.
        ens_vals <- setNames(as.numeric(unlist(scen_ens_impute)), temps_list_names)
        stopifnot(length(ens_vals) == length(temps_list_names))
        row_add <- data.frame(
          scenario = scen,
          GCM = "ensemble_mean",
          as.list(ens_vals),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        climate_anom <- rbind(climate_anom, row_add)

      } else {
        cat(paste0("\nNo complete GSAT values found for ", scen, ": skipping"),
            file = logfile_build, append = TRUE)
      }

    }

  }

  gsat_numeric <- vapply(temps_list_names, function(nm) {
    is.numeric(climate_anom[[nm]])
  }, logical(1))
  stopifnot(
    "calc_temps_gcms: GSAT columns must remain numeric" = all(gsat_numeric)
  )

  cat( "\n\ncalc_temps_gcms: returning temperature change(s)\n", file = logfile_build, append = TRUE )
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  # nrows(climate_data) x {scenario, GCM, anom}
  return(climate_anom)

}
