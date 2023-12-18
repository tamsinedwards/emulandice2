#' do_calibration: calculate calibration information.
#'
#' @description
#' Use distance of each emulator prediction over the historical period from
#' observations to calculate whether it is implausible/NROY (history matching),
#' or calculate weights using a Gaussian likelihood (Bayesian calibration).
#'
#' @param distances Distance of each historical emulator prediction from the
#' observations
#' @param calib_type history_matching or Bayesian
#'
#' @returns `do_calibration()` returns index of NROY ensemble members if
#' history matching or weights for each ensemble member if Bayesian calibration.
#'
#' @export

do_calibration <- function(distances, calib_type) {

  if (calib_type == "history_matching") {
    cat("\n_________________________________\n",file = logfile_results, append = TRUE)
    cat(paste("do_calibration:", calib_type, "\n"), file = logfile_results, append = TRUE)
  }

  if (calib_type == "history_matching") {

    # Create NROY index
    nroy_index <- which( abs(distances/obs_change_err) < 3 )

    cat(sprintf("Selected %i (%.1f%%) of %i ensemble members from %s scenario\n",
                length(nroy_index), length(nroy_index)*100/length(distances),
                length(distances), scen), file = logfile_results, append = TRUE)

    cat("_________________________________\n",file = logfile_results, append = TRUE)

    return(nroy_index)

  }
  if (calib_type == "Bayesian") {

    # Calculate weights with Gaussian univariate likelihood
    weightsraw <- sapply( distances, function(x) exp( -0.5 * sum( x * x , na.rm=TRUE) / (obs_change_err * obs_change_err) ) )
    weights <- weightsraw / sum( weightsraw )

    return(weights)
  }



}

# Get NROY index for FINAL projections
#    proj_nroy <- list()
#    for (scen in scenario_list) {
#   proj_nroy[[scen]] <- which( abs(dist_proj/obs_change_err) < 3 )
#     print(sprintf("Selected %i (%.1f%%) of %i final ensemble members from %s scenario",
#                  length(proj_nroy[[scen]]),
#                  length(proj_nroy[[scen]])*100/N_temp, N_temp, scen))
#  }


# OBSOLETE: Interval-based history matching
#obs_max <- obs_change + 3 * total_err[obs_data$Year == cal_end]
#obs_min <- obs_change - 3 * total_err[obs_data$Year == cal_end]


#myem_nroy[[scen]] <- which( myem[[scen]]$mean[, paste0("y",cal_end)] < obs_max & myem[[scen]]$mean[,  paste0("y",cal_end)] > obs_min )

#  proj_nroy[[scen]] <- which( projections[[scen]][, paste0("y",cal_end) ] < obs_max &
#                                projections[[scen]][, paste0("y",cal_end)] > obs_min )
