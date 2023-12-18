#' match_sims: match ice simulations to climate forcing simulations
#'
#' @description
#' Look up climate simulation for each ice simulation by SSP and GCM, check
#' it exists and is long enough, and calculate climate change for emulator input.
#'
#' @returns `match_sims()` returns requested temperature change(s) for each
#' simulation.
#'
#' @export

match_sims <- function() {

  # xxx  Check dim of climate_exp is [1, n] ?

  # Match climate and ice sims ---------------------------------------------------------------------

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("match_sims: matching ice simulations with forcing simulations\n\n",
      file = logfile_build, append = TRUE)

  # Lookup global mean temperature (GSAT) for each scenario/GCM combination
  # and calculate anomalies

  # This is used in ice_design
  # Single timeslice is for backward compatibility
  if (length(temps_list) == 1) {
    temps <- rep(NA, dim(ice_data)[1])
  } else {
    temps <- matrix( NA, nrow = dim(ice_data)[1], ncol = length(temps_list) )
    colnames(temps) <- paste0("GSAT_", temps_list)
  }

  # Drop erroneous last col of NAs
  climate_data <- climate_data[ , -dim(climate_data)[2]]

  # To store all years of selected forcings for plots
  climate_exp_all <- NA


  # Find scenario/GCM forcing pair for each land ice simulation
  for ( ss in 1:dim(ice_data)[1] ) {

    # Look for forcing
    # This is a 1 x years matrix
    climate_exp <- climate_data[ climate_data$GCM == ice_data[ ss, "GCM" ]
                                 & climate_data$scenario == ice_data[ ss,"scenario"], ]

    # Warn and skip if no forcing found
    # unless it's a control simulation
    if ( dim(climate_exp)[1] == 0 ) {

      if ( ice_data[ ss,"scenario"] != "ctrl" )
        cat(paste(ss,": could not find forcing for", ice_data[ ss,"scenario"],
                  ice_data[ ss, "GCM" ],"- skipping run\n"), file = logfile_build, append = TRUE)

    }

    # If found forcing
    else {

      # If 2100 value not found
      if ( is.na(climate_exp[ , "y2100"]) ) {

        # If 2099 value is there (quite common), then impute
        if ( !is.na(climate_exp[ , "y2099"]) ) {
          #        print( paste(ss,": forcing data missing in 2100 for",
          #                     ice_data[ ss,"scenario"],
          #                     ice_data[ ss, "GCM" ],"- imputing with 2099") )
          climate_exp[ , "y2100" ] <- climate_exp[ , "y2099" ]

        } else {

          # Skip: this shouldn't happen, as all runs have data to 2099
          # in 230607 forcing dataset,
          # but keep in case new forcing rows do
          cat( paste(ss,": forcing data missing in 2099 for",
                     ice_data[ ss,"scenario"],
                     ice_data[ ss, "GCM" ],"- skipping run\n"),file = logfile_build, append = TRUE)
        }
      } # if no 2100

      # Ditto for 2300
      if ( is.na(climate_exp[ , "y2300"]) ) {

        # If 2299 value is there, then impute
        if ( !is.na(climate_exp[ , "y2299"]) ) {
          # print( paste(ss,": forcing data missing in 2300 for",
          #              ice_data[ ss,"scenario"],
          #              ice_data[ ss, "GCM" ],"- imputing with 2299") )
          climate_exp[ , "y2300" ] <- climate_exp[ , "y2299" ]

        } # No need to warn otherwise, as lots of runs don't go beyond 2100

      } # if no 2300

      # GIS: FIXED CLIMATE NOT SSP FROM 2100 for most runs
      if ( i_s == "GIS" && !is.na(ice_data[ss, "fixed_date"]) && ice_data[ss, "fixed_date"] == 2100 ) {

        # print(paste(ss,": creating fixed climate from 2100 for",
        #             ice_data[ ss,"scenario"],
        #             ice_data[ ss, "GCM" ]))

        # Index for each decade after fixed date
        decadal_ind <- seq(from = 2101, to = 2291, by = 10)

        # Paste 2091-2100 values into these
        for (dd in 1:length(decadal_ind)) {
          climate_exp[ , paste0("y", decadal_ind[dd] + 0:9)] <- climate_exp[ , paste0("y", 2091:2100)]
        }
      }

      # Subtract baseline from whole time series
      temps_period <- temps_baseline + 1:N_temp_yrs - 1
      climate_exp[ , paste0("y",1850:2300) ] <- unlist(climate_exp[ , paste0("y",1850:2300) ]) - mean(unlist(climate_exp[ , paste0( "y", temps_period) ]))
      # unlist(climate_exp[  , paste0( "y", temps_baseline ) ])

      if (ss == 1) {
        cat( paste("GSAT: baseline mean period", paste(range(temps_period), collapse = "-"), "\n"),
             file = logfile_build, append = TRUE )
      }

      # Store whole GSAT time series for plots
      if ( length(climate_exp_all) == 1 && is.na(climate_exp_all) ) {
        climate_exp_all <- climate_exp
      } else climate_exp_all <- rbind(climate_exp_all, climate_exp)

      # Keep temps we want
      if ( length(temps_list) == 1 ) {

        temps_period <- temps_list - N_temp_yrs:1 + 1
        temps[ ss ] <- mean(unlist(climate_exp[ , paste0( "y", temps_period) ]))

        if (ss == 1) {
          cat( paste("GSAT: forcing mean period", paste(range(temps_period), collapse = "-"), "\n"),
               file = logfile_build, append = TRUE )
        }
      } else {

        for ( tt in temps_list) {
          temps_period <- tt - N_temp_yrs:1 + 1
          temps[ ss, ] <- mean(unlist(climate_exp[ , paste0( "y", temps_period) ]))
          if (ss == 1 ) {
            cat( paste("GSAT: forcing mean period", paste(range(temps_period), collapse = "-"), "\n"),
                 file = logfile_build, append = TRUE )
          }
        }

      }

    } # found forcing file

  } # land ice simulations loop

  if (length(temps[is.na(temps)]) > 0) cat(paste("MISSING", length(temps[is.na(temps)]), "FORCING SIMULATIONS\n"),
                                           file = logfile_build, append = TRUE)

  cat(paste("\nmatch_sims: found",length(temps[!is.na(temps)]),"of",length(temps),
            "forcing simulations\n"), file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  return(temps)

}
