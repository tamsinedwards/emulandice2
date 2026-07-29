#' select_sims: select land ice simulations
#'
#' @description
#' Select land ice simulations from ice data file: ice source, region, model(s),
#' sufficient length, and further ice-source-specific selections.
#'
#' @param select_type Type of selection: main or history_match
#' @returns select_sims returns a matrix ice_data which is a subset of the
#' original, if select_type is main, or else row index if history_match
#'
#' @export

select_sims <- function(select_type) {

  n_presel <- nrow(ice_data)

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat( sprintf("select_sims: selecting simulations from ice data file - %s\n", select_type),
       file = logfile_build, append = TRUE)

  # Main selections  ------------------------------------------------------------

  # MAIN SELECTIONS
  if (select_type == "main") {

    cat( paste0("\nNumber of ice simulations in file: ", dim(ice_data)[1], "\n"),
         file = logfile_build, append = TRUE)

    # Get land ice type and region(s)
    ice_data <- ice_data[ice_data$ice_source == i_s & ice_data$region == reg, ]
    cat( paste0("\nNumber of ice simulations in file for ",i_s," ",reg,": ", dim(ice_data)[1], "\n"),
         file = logfile_build, append = TRUE)

    # If model_list is specified, use this to select
    if ( length(model_list) > 1 || (length(model_list) == 1 && !is.na(model_list)) ) {
      ice_data <- ice_data[ice_data$model %in% model_list, ]
      cat( paste0("After selecting model(s) ", paste(model_list, collapse = " "),": ", dim(ice_data)[1],"\n"),
           file = logfile_build, append = TRUE)
    }

    if (FALSE) {
      # Impute a few missing years in BISICLES by hand
      # Could do this with auto-impute? xxx
      # Last 1-2 years: repeat
      miss_ind <- ice_data$model == "BISICLES" & is.na(ice_data$y2299) & !is.na(ice_data$y2298)
      if (length(miss_ind[miss_ind]) > 0) {
        cat(sprintf("Imputing %i BISICLES simulations by setting 2299 to 2298 value\n", length(miss_ind[miss_ind])),
            file = logfile_build, append = TRUE)
        ice_data[ miss_ind, "y2299"] <- ice_data[ miss_ind, "y2298"]
      }
      miss_ind <- ice_data$model == "BISICLES" & is.na(ice_data$y2300) & !is.na(ice_data$y2299)
      if (length(miss_ind[miss_ind]) > 0) {
        cat(sprintf("Imputing %i BISICLES simulations by setting 2300 to 2299 value\n", length(miss_ind[miss_ind])),
            file = logfile_build, append = TRUE)
        ice_data[ miss_ind, "y2300"] <- ice_data[ miss_ind, "y2299"]
      }
      # XXX Remove later if dataset fixed
      # 2144, 2289: interpolate
      miss_ind <- ice_data$model == "BISICLES" & is.na(ice_data$y2144) & !is.na(ice_data$y2145)
      if (length(miss_ind[miss_ind]) > 0) {
        cat(sprintf("Imputing %i BISICLES simulations by setting 2144 to interpolated value\n", length(miss_ind[miss_ind])),
            file = logfile_build, append = TRUE)
        ice_data[ miss_ind, "y2144"] <- apply(ice_data[miss_ind,c("y2143","y2145")], 1, mean)

      }
      miss_ind <- ice_data$model == "BISICLES" & is.na(ice_data$y2289) & !is.na(ice_data$y2290)
      if (length(miss_ind[miss_ind]) > 0) {
        cat(sprintf("Imputing %i BISICLES simulations by setting 2289 to interpolated value\n", length(miss_ind[miss_ind])),
            file = logfile_build, append = TRUE)
        ice_data[ miss_ind, "y2289"] <- apply(ice_data[miss_ind,c("y2288","y2290")], 1, mean)
      }

    } # BISICLES: FALSE

    # Target year: end of simulation, or allow to be shorter if imputing
    end_year <- final_year - impute_nyrs

    # (If not imputing, just check last year)
    if ( impute_nyrs == 0 || impute_sims == "none" ) {

      last_col <- ice_data[ , paste0("y", final_year) ]
      found_val <- ifelse( length(last_col[ !is.na(last_col) ]) > 0, TRUE, FALSE)

    } else {

      # Keep simulations if least one non-missing value in columns end_year-final_year
      last_cols <- ice_data[ , paste0("y", end_year:final_year) ]

      found_val <- apply(last_cols, 1, function(x) {
        ifelse( length(x[ !is.na(x) ]) > 0, TRUE, FALSE)
      })
    }

    ice_data <- ice_data[ found_val, ]

    cat( paste0("After checking simulations have non-missing values up to (or beyond) ", end_year, ": ", nrow(ice_data),"\n"),
         file = logfile_build, append = TRUE)

    #__________________________________________________
    # GREENLAND SELECTIONS
    if (i_s == "GIS") {

      # Exclude control runs from GIS xxx just put under deliverable_test
      ice_data <- ice_data [ ice_data$scenario != "ctrl", ]
      cat( paste("After removing GIS control simulations:", dim(ice_data)[1],"\n"),
           file = logfile_build, append = TRUE)

      # Greenland CISM-only: select best runs (retreat same values in historical and future)
      if (need_retreat_match) {

        # Select CISM simulations with matching retreat, or other model runs
        ice_data <- ice_data[ (ice_data$model == "CISM" & ice_data$is_hist_match) | (ice_data$model != "CISM"), ]

        cat( paste("After rejecting CISM projections with non-matching retreat in historical:", dim(ice_data)[1],"\n"),
             file = logfile_build, append = TRUE)

      }


    }

    #__________________________________________________

    # ANTARCTIC SELECTIONS

    if ( i_s == "AIS") {

      # Faster if drop big GCM-forced ensembles...
      if (ensemble_subset == "RCM_forced") {
        ice_data <- ice_data[ ice_data$forcing_type == "RCM", ]
        cat( paste("After selecting only RCM-forced:", dim(ice_data)[1], "\n"),
             file = logfile_build, append = TRUE)
      }

      # GCM-forced only
      if (ensemble_subset == "GCM_forced") {
        ice_data <- ice_data[ ice_data$forcing_type == "GCM", ]
        cat( paste("After selecting only GCM-forced:", dim(ice_data)[1], "\n"),
             file = logfile_build, append = TRUE)
      }


    }

    #__________________________________________________
    # GLACIER SELECTIONS

    if ( i_s == "GLA" ) {

      # Excluding using data quality flag "complete":

      # Index to keep (GloGEM is NA, and so is OGGM 2100 forcing ensemble: keep these)
      complete_sel <- is.na(ice_data$complete) |
        (ice_data$model == "OGGM" & ice_data$complete >= complete_thresh[["OGGM"]]) |
        (ice_data$model == "GO" & ice_data$complete >= complete_thresh[["GO"]])

      # Restrict dataset
      ice_data <- ice_data[ complete_sel , ]

      cat( sprintf("\nAfter applying completion thresholds of %.0f%% to OGGM and %.0f%% to GO: %i\n",
                   100.0*complete_thresh[["OGGM"]], 100.0*complete_thresh[["GO"]], dim(ice_data)[1]),
           file = logfile_build, append = TRUE )

    }

  } # select_type == main

  # History matching  ------------------------------------------------------------

  # Broad history matching before emulation
  if (select_type == "history_match") {

    # Pre-screening with history matching:

    # Broad history matching, using slightly tailored thresholds
    # use _sel to avoid confusion with later projection calibration

    # Model discrepancy scaling factor for pre-screening
    scale_mod_err_sel <- 5.0

    # Total error
    total_err_sel <- sqrt(obs_err^2 + ( scale_mod_err_sel * obs_err )^2)

    # Sea level change over same period
    model_change <- ( ice_data[ , paste0("y",cal_end) ]
                      - ice_data[ , paste0("y",cal_start) ] )

    # Implausibility
    implausibility <- abs( (model_change - obs_change) / total_err_sel )

    # Threshold (by Pukelsheim)
    imp_thresh <- 6


    # Apply threshold
    nroy_sel <- implausibility <= imp_thresh

    # Select ice and climate forcing datasets
    # xxx now only used for below because nroy_sel is returned
    ice_data <- ice_data[ nroy_sel , ]

    cat("\nselect_sims: history matching\n", file = logfile_build, append = TRUE)
    cat(sprintf("select_sims: model error %i x obs error\n", scale_mod_err_sel), file = logfile_build, append = TRUE)
    cat(sprintf("select_sims: threshold (Pukelsheim): %.1f\n", imp_thresh), file = logfile_build, append = TRUE)
    cat(sprintf("select_sims: observed sea level change (cm SLE, %s):\n", obs_period), file = logfile_build, append = TRUE)
    cat(sprintf("select_sims: %.4f +/- %.4f cm SLE (+/- %.1f s.d total error)\n", obs_change, imp_thresh*total_err_sel, imp_thresh), file = logfile_build, append = TRUE)
    cat(sprintf("select_sims: from %.4f to %.4f cm SLE\n", obs_change - imp_thresh*total_err_sel, obs_change + imp_thresh*total_err_sel), file = logfile_build, append = TRUE)

    cat( sprintf("\nAfter restricting to |I| < %i (with model discrep x %i obs_error): %i\n",
                 imp_thresh, scale_mod_err_sel, dim(ice_data)[1]),
         file = logfile_build, append = TRUE )

  } # history_match

  #__________________________________________________
  # SUMMARY

  cat(paste("\nselect_sims: SELECTED", dim(ice_data)[1], "of",n_presel,"ICE SIMULATIONS FOR", i_s, reg, "\n"),
      file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  # Return ice_data or NROY index
  # xxx would be clearer to split into two functions, or else
  # rewrite main to return index of original
  if (select_type == "main") return(ice_data)
  if (select_type == "history_match") return(nroy_sel)

}
