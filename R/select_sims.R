#' select_sims: select land ice simulations
#'
#' @description
#' Select land ice simulations from ice data file: ice source, region, model(s),
#' sufficient length, and further ice-source-specific selections.
#'
#' @param select_type Type of selection: main or history_match
#' @returns select_sims returns a matrix ice_data which is a subset of the original.
#'
#' @export

select_sims <- function(select_type) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat( sprintf("select_sims: selecting simulations from ice data file - %s\n", select_type),
       file = logfile_build, append = TRUE)

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

    # Select runs that get to final expected year (unless imputing)
    if ( ! impute_sims ) {
      full_length <- ! is.na( ice_data[ , paste0("y", years_sim[ length(years_sim) ] ) ] )
      ice_data <- ice_data[ full_length, ]
      cat( paste0("After checking simulations reach ",years_sim[ length(years_sim) ], ": ", dim(ice_data)[1],"\n"),
           file = logfile_build, append = TRUE)

    }

    #__________________________________________________
    # GREENLAND SELECTIONS
    if (i_s == "GIS") {

      # Exclude control runs from GIS
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

    if ( i_s == "GLA" && !is.na(complete_thresh) ) {

      # Excluding using data quality flag "complete":

      # Index to keep (GloGEM is NA so don't test)
      complete_sel <- ice_data$complete >= complete_thresh | is.na(ice_data$complete)

      # Restrict dataset
      ice_data <- ice_data[ complete_sel , ]

      cat( sprintf("\nAfter restricting OGGM to >= %.0f%% complete: %i\n",
                   100.0*complete_thresh, dim(ice_data)[1]),
           file = logfile_build, append = TRUE )

    }

    #__________________________________________________
    # Subset simulations for testing

    #  if ( ! is.na(target_size) & dim(ice_data)[1] > target_size ) {

    # Randomly sample
    #    sel_index <- sort(sample(nrow(ice_data), target_size))

    #      ice_data <- ice_data[ sel_index, ]
    #      cat( paste("After randomly selecting",target_size,"simulations to limit size:", dim(ice_data)[1], "\n"),
    #          file = logfile_build, append = TRUE)

    #    }


  } # select_type == main


  # Broad history matching for glacier OGGM simulations
  if (select_type == "history_match") {

    if (i_s == "GLA") {

      # Pre-screening with history matching:

      # Broad history matching, using slightly tailored thresholds
      # xxx Temporary code until consolidating more neatly
      # use _sel to avoid confusion with later projection calibration

      # Model discrepancy scaling factor for glacier pre-screening only
      scale_mod_err_sel <- 10

      # Total error
      model_err_sel <- scale_mod_err_sel * obs_data[,"SLE_sd"]
      total_err_sel <- sqrt(obs_data[,"SLE_sd"]^2 + model_err_sel^2)

      # Change and error for calibration period
      obs_change_sel <- obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
      obs_change_err_sel <- total_err_sel[obs_data$Year == cal_end]

      # Sea level change
      # xxx cal_start part should be redundant because (currently) zero
      model_change <- ( ice_data[ , paste0("y",cal_end) ]
                        - ice_data[ , paste0("y",cal_start) ] )

      # Implausibility
      implausibility <- abs( (model_change - obs_change_sel) / obs_change_err_sel )

      # Default threshold (5 large and 5 small regions)
      imp_thresh <- 50

      # Adjust for 9 of 19 regions
      # LARGE (vol > 1 cm SLE)
      if (reg_num == 4) imp_thresh <- 30 # Arctic Canada South
      if (reg_num == 9) imp_thresh <- 100 # Russia
      if (reg_num == 19) imp_thresh <- 150 # AIS

      # SMALL
      if (reg_num %in% c(10, 11, 12, 18)) imp_thresh <- 30
      if (reg_num == 13) imp_thresh <- 60
      if (reg_num == 14) imp_thresh <- 100

      # Index to keep
      # Apply just to OGGM
      #      nroy_sel <- (implausibility <= imp_thresh & ice_data$model == "OGGM") |
      #        ice_data$model %in% model_list[ model_list != "OGGM"]

      # Apply to both models
      nroy_sel <- implausibility <= imp_thresh

      # Restrict dataset
      ice_data <- ice_data[ nroy_sel , ]

      #cat( sprintf("\nAfter restricting OGGM to I < %i (model discrep x %i obs_error): %i\n",
      #             imp_thresh, scale_mod_err_sel, dim(ice_data)[1]),
      #     file = logfile_build, append = TRUE )

      cat( sprintf("\nAfter restricting to I < %i (model discrep x %i obs_error): %i\n",
                   imp_thresh, scale_mod_err_sel, dim(ice_data)[1]),
           file = logfile_build, append = TRUE )


    } # GLA

  } # history_match

  #__________________________________________________
  # SUMMARY

  cat(paste("\nselect_sims: SELECTED", dim(ice_data)[1], "ICE SIMULATIONS FOR", i_s, reg, "\n"),
      file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  return(ice_data)

}
