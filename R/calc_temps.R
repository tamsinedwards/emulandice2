#' calc_temps: calculate climate change metric for emulator input
#'
#' @description
#' Calculate climate change metric(s) for emulator input from climate forcings
#'
#' @returns `calc_temps()` returns requested temperature change(s) for all
#' simulations in climate forcing file.
#'
#' @export

calc_temps <- function(climate_dataset, mean_impute = FALSE) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat( "calc_temps: calculating temperature change(s) for all forcings in dataset\n", file = logfile_build, append = TRUE )
  if (mean_impute) cat( "Imputing missing GCMs with mean of others for SSP\n\n",
                        file = logfile_build, append = TRUE )

  # Baseline period
  temps_period <- temps_baseline + 1:N_temp_yrs - 1

  cat( paste("Calculating GSAT mean for baseline period:", paste(range(temps_period), collapse = "-"), "\n"),
       file = logfile_build, append = TRUE )

  # Drop scenario, vals before calculating just to save indexes (re-add after)
  climate_vals <- climate_dataset[ , -c(1,2) ]

  # Normalise by subtracting baseline mean from whole time series XXXX plot this?
  climate_norm <- t( apply(climate_vals, 1, function(x) {
    x <- unlist(x) - mean( unlist(x[ paste0( "y", temps_period) ]) )
  }) )

  # Calculate temp change(s) we want for emulator
  # Either vector or array, depending on how many GSAT timeslices requested
  if ( length(temps_list) == 1 ) {

    temps_period_future <- temps_list - N_temp_yrs:1 + 1

    cat( paste("Calculating GSAT mean for future period:", paste(range(temps_period_future), collapse = "-"), "\n"),
         file = logfile_build, append = TRUE )

    climate_anom <-  apply(climate_norm, 1, function(x) {
      x <- mean( unlist( x[ paste0( "y", temps_period_future) ] ) ) })


  } else {

    climate_anom <- matrix( NA, nrow = dim(climate_norm)[1], ncol = length(temps_list) )

    # For each GSAT timeslice
    for ( tt in 1:length(temps_list)) {

      temps_period_future <- temps_list[tt] - N_temp_yrs:1 + 1

      cat( paste("GSAT: future mean period", paste(range(temps_period_future), collapse = "-"), "\n"),
           file = logfile_build, append = TRUE )

      climate_anom[ , tt] <- apply(climate_norm, 1, function(x) {
        mean( unlist(x[ paste0( "y", temps_period_future) ]) )
      })

    }

  }

  # Add scenario and GCM columns back
  climate_anom <- cbind( climate_dataset[ , 1:2 ], climate_anom )
  colnames(climate_anom)[-c(1,2)] <- temps_list_names

  # Add extra rows for ensemble mean if imputing simulations
  if (mean_impute) {

    # Checking  final GSAT
    tt_last <- temps_list_names[length(temps_list_names)]

    # For each scenario in dataset
    for ( scen in unique(climate_dataset[ , "scenario" ])) {

      # Get rows with non-missing final value for SSP
      scen_ens <- climate_anom[ climate_anom$scenario == scen &
                                  ! is.na(climate_anom[, tt_last ]), ]

      # If some GCMs exist for this SSP
      if ( dim(scen_ens)[1] > 0) {

        cat(paste0("\n\nFound ", tt_last, " for ",dim(scen_ens)[1],
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

        # Append to main dataset
        climate_anom <- rbind( climate_anom, c(scen, "ensemble_mean", scen_ens_impute))

      } else {
        cat(paste0("\nNo complete GSAT values found for ", scen, ": skipping"),
            file = logfile_build, append = TRUE)
      }

    }

  }

  cat( "\n\ncalc_temps: returning temperature change(s)\n", file = logfile_build, append = TRUE )
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  return(climate_anom)

}
