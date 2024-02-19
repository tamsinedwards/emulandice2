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
    ice_data <- ice_data [ ice_data$scenario != "ctrl", ]
    cat( paste("After removing GIS control simulations:", dim(ice_data)[1],"\n"),
         file = logfile_build, append = TRUE)

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

  if ( i_s == "AIS") { # dataset == "PROTECT" &&

    # Drop Phase 1 Kori
    if ( "Kori" %in% model_list && ensemble_subset %in% c("GCM_forced", "all_forced")) {

      # Only Kori has Phase = 1 set
      ice_data <- ice_data[ ice_data$Phase != 1 | is.na(ice_data$Phase), ]
      cat( paste("After dropping Kori Phase 1:", dim(ice_data)[1], "\n"),
           file = logfile_build, append = TRUE)

    }

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

  # Restrict temp_bias
  if ( i_s == "GLA" ) {

    # Get range of temp_bias in OGGM ensemble
    min_tb <- min(ice_data[ ice_data$model == "OGGM", "temp_bias" ])
    max_tb <- max(ice_data[ ice_data$model == "OGGM", "temp_bias" ])
    range_tb <- max_tb - min_tb

    cat( sprintf("Restricting OGGM temp_bias range from original range (%.2f to %.2f)\n",
               min_tb, max_tb ),
         file = logfile_build, append = TRUE)

    # Reduce range to 20%, i.e. +/- 1degC not 5degC from regional mean of tuned values
    min_tb <- min_tb + 0.4*range_tb
    max_tb <- max_tb - 0.4*range_tb

    cat( sprintf("to new range (%.2f to %.2f): ", min_tb, max_tb),
         file = logfile_build, append = TRUE )

    # Restrict for OGGM only
    ice_data <- ice_data[ ( ice_data$model == "OGGM" &
                              ice_data$temp_bias >= min_tb & ice_data$temp_bias <= max_tb )
                          | ice_data$model %in% model_list[ model_list != "OGGM"], ]

    cat( paste(dim(ice_data)[1], "\n"),
         file = logfile_build, append = TRUE)

    # Restrict precip_corr_factor to maximum value
    # Based on comparison with observations (and GloGEM max is 2.2)
    pcf_max <- 4.0

    # Restrict for OGGM only
    ice_data <- ice_data[ ( ice_data$model == "OGGM" &
                              ice_data$prec_corr_factor <= pcf_max )
                          | ice_data$model %in% model_list[ model_list != "OGGM"], ]

    cat( sprintf("After restricting OGGM precip_corr_factor range to < %.1f: %i\n",
                 pcf_max, dim(ice_data)[1]),
         file = logfile_build, append = TRUE)

  }

  #__________________________________________________
  # Subset simulations for testing

  if ( ! is.na(target_size) & dim(ice_data)[1] > target_size ) {

    #sel_num <- ceiling( dim(ice_data)[1] / target_size )

    # Randomly sample
    sel_index <- sort(sample(nrow(ice_data), target_size))

    #ice_data <- ice_data[ seq(from = 1, to = nrow(ice_data), by = sel_num), ]
    ice_data <- ice_data[ sel_index, ]
    cat( paste("After randomly selecting",target_size,"simulations to limit size:", dim(ice_data)[1], "\n"),
         file = logfile_build, append = TRUE)
  }

  #__________________________________________________
  # SUMMARY

  cat(paste("\nselect_sims: SELECTED", dim(ice_data)[1], "ICE SIMULATIONS FOR", i_s, reg, "\n"),
      file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  return(ice_data)

}
