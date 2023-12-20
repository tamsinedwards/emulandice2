#' select_sims: select land ice simulations
#'
#' @description
#' Select land ice simulations from ice data file: ice source, region, model(s),
#' sufficient length, and further ice-source-specific selections.
#'
#' @returns select_sims returns a matrix ice_data which is a subset of the original.
#'
#' @export

select_sims <- function() {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("select_sims: selecting simulations from ice data file\n",file = logfile_build, append = TRUE)

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

  # Select runs that get to final expected year
  full_length <- ! is.na( ice_data[ , paste0("y", years_sim[ length(years_sim) ] ) ] )
  ice_data <- ice_data[ full_length, ]
  cat( paste0("After checking simulations reach ",years_sim[ length(years_sim) ], ": ", dim(ice_data)[1],"\n"),
       file = logfile_build, append = TRUE)

  #__________________________________________________
  # GREENLAND SELECTIONS
  if (i_s == "GIS") { # %in% c("GIS","GrIS")) {

    # Exclude ISMIP6 open melt runs for now
    # xxx Remove line and replace with dummy variable in emulator
    #if ( dataset == "IPCC_AR6" && i_s == "GrIS") {
    #  ice_data <- ice_data[ !is.na(ice_data$melt), ]
    #  cat( paste("After excluding ISMIP6 open melt:", dim(ice_data)[1], "\n"), file = logfile_build, append = TRUE)
    #}

    # Exclude control runs from GIS
    # xxx add "if PROTECT"?
    if (i_s == "GIS") {
      ice_data <- ice_data [ ice_data$scenario != "ctrl", ]
      cat( paste("After removing GIS control simulations:", dim(ice_data)[1],"\n"),
           file = logfile_build, append = TRUE)
    }


    # Greenland CISM-only: select best (matching historical)
    # xxx add "if PROTECT"?
    if (need_hist_match) {

      # Select CISM simulations with matching retreat, or other model runs
      ice_data <- ice_data[ (ice_data$model == "CISM" & ice_data$is_hist_match) | (ice_data$model != "CISM"), ]

      cat( paste("After rejecting CISM projections with non-matching retreat in historical:", dim(ice_data)[1],"\n"),
           file = logfile_build, append = TRUE)

    }

  }

  #__________________________________________________

  # ANTARCTIC SELECTIONS

  # xxx Obsolete: only use PICO
  if ( i_s == "AIS") { # dataset == "PROTECT"
    if ( "Kori" %in% model_list && !is.na(melt_param_select))  {
      ice_data <- ice_data[ ice_data$melt_param == melt_param_select, ]
      cat( paste("After selecting melt parameterisation for Kori:", dim(ice_data)[1], "\n"),
           file = logfile_build, append = TRUE)
    }
  }

  # XXX quick hack for faster AIS LOO - get rid of this / improve
  if ( i_s == "AIS") { # dataset == "PROTECT" &&
    if ( "Kori" %in% model_list && do_loo_validation)  {
      ice_data <- ice_data[ seq(from = 1, to = nrow(ice_data), by = 4), ]
      cat( paste("After selecting every 4th simulation:", dim(ice_data)[1], "\n"),
           file = logfile_build, append = TRUE)
    }
  }

  #__________________________________________________
  # GLACIER SELECTIONS

  # XXX Aim to make this obsolete later by combining ensembles
  if (FALSE) {
    if ( i_s == "GLA" ) {
      if (ensemble_subset == "PPE") {
        ice_data <- ice_data[ ! is.na(ice_data$prec_corr_factor), ]
        cat(paste("After selecting PPE(s):", dim(ice_data)[1],"\n"), file = logfile_build, append = TRUE)

      } else {
        ice_data <- ice_data[ is.na(ice_data$prec_corr_factor), ]
        cat(paste("After selecting forcing ensemble(s):", dim(ice_data)[1],"\n"),file = logfile_build, append = TRUE)
      }
    }
  }

  #__________________________________________________
  # SUMMARY

  cat(paste("\nselect_sims: SELECTED", dim(ice_data)[1], "ICE SIMULATIONS FOR", i_s, reg, "\n"),
      file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  return(ice_data)

}
