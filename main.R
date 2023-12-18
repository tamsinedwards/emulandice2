#' ---
#' title: "emulandice2: predict"
#' output:
#'    html_notebook:
#'      toc: true
#'      number_sections: true
#'
#' ---
#' MULTIVARIATE EMULATION OF LAND ICE CONTRIBUTIONS TO SEA LEVEL
# Based on multivariate emulator code by Jonathan Rougier
#
# Datasets:
# - IPCC AR6: 2015-2100 (less tested - maybe deleting too)
# - PROTECT: various dates (1950-2005) to 2300
#
# CSV output files: nrows = GSAT samples (2237 for AR6) x years_em timeslices
#_______________________________________________________________________________

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SETTINGS: FACTS COMMAND LINE
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Get arguments from RScript
args <- commandArgs(TRUE)

# ice_source
print(paste("Ice source:", args[1]))
i_s <- args[1]
stopifnot(i_s %in% c("GIS", "AIS", "GLA"))

# Region of ice_source
print(paste("Region:", args[2]))
reg <- args[2] # region
stopifnot(reg %in% c("ALL", paste0("RGI", sprintf("%02i",1:19))))

# Emulator: this is made from model_list and emulator_settings
emu_name <- args[3]


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SETTINGS: TO ADD TO FACTS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# CLIMATE FORCING: same as emulandice v1 AR6, but with projections to 2300
# Used in load_design_to_pred.R

# CSV
# scm_file <- "CLIMATE_FORCING_IPCC_AR6_230706.csv"

# netcdf
ssp <- "585"
scm_ssp <- paste0("ssp",ssp) # FACTS uses lower case
scenario <- paste0("SSP",ssp) # emulandice expects upper case
scm_file <- paste0("emulandice.",scm_ssp,".temperature.fair.temperature_climate.nc")


# Time period for predictions, e.g. 10 for decadal, and baseline year
# Not obvious

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SETTINGS: MAYBE ADD TO FACTS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Write mean emulator projections as well as full projections with noise
# (used as arg for write_outputs(), and in plot_bayesian.R for some reason)
write_mean <- FALSE


#' # Start analysis
# Start analysis ------------------------------------------------------------

# EMULATOR BUILD FILE: constructed from the above settings
# Directory has to match outdir when built
emu_file <- paste0("../RESULTS/tmp/", paste(i_s, reg, emu_name, sep = "_"), "_EMULATOR.RData")

cat(sprintf("Looking for emulator build file: %s\n", emu_file))
stopifnot(file.exists(emu_file))
cat("Loading build file\n")


# LOAD EMULATOR AND OTHER STUFF
load( file = emu_file)

# xxx NEED TO CHECK SCENARIO_LIST DESIGN STUFF
stopifnot(scenario %in% scenario_list)
print(paste("Predicting for scenario:", scenario))
scenario_list <- scenario

# Number of 2LM projections of GSAT expected per SSP
# (and therefore total number of samples for book-keeping by GSAT value)
# Checks when reading in netcdf - could get rid of this
N_2LM <- 50L # Was 2237L

cat("Running...\n")


# START OUTPUT FILES

#' # Open output text file
# Open output files ------------------------------------------------------------

# Log file from emulator_build.R is same name but _build.txt
logfile_results <- paste0(outdir, "/",out_name,"_results.txt")
cat(sprintf("\nemulandice2: %s %s\n\n", i_s, reg), file = logfile_results)

cat(sprintf("\nLoaded emulator file: %s\n", emu_file), file = logfile_results, append = TRUE)


# +++++++++++++++
# TO DO
# - set random seed?
# - tidy logfile names
# - move ME plots into emulator_build.R
#
# FACTS
# - Input time period and start date for predictions e.g. 10 for decadal, 2000 for first year
# --- this takes thought because built into emulator: is 5 yearly enough?
# - Read GSAT netcdf, not CSV
# - Output FACTS sea level netcdfs: one per region (i.e. do spatial correlations later)
# - Remove IPCC legacy code

#' # Designs
# Design -----------------------------------------------------------------------
# FACTS: will need to read in any GSAT projections


#' ## SSPs
# Future projections
# xxx i.e. scm_file is used in this function -> change to arg
design_pred <- load_design_to_pred( design_name )

# Store number of samples per scenario
# Used for outputs
N_temp <- length( design_pred[[1]][ , 1] )

#' # Predict
# Predict ----------------------------------------------------------------------
# emulator_predict() calls emu_mv with type = "var"
# FACTS: use emulator object saved to RData workspace file

# myem <- list() # this is in RData file with ME projections

# Rescale priors using same scaling factors as for simulator inputs
# Not the most elegant

cat("\nPredict for design:\n", file = logfile_results, append = TRUE)

#' ## SSPs: uniform GSAT or 2LM
for (scen in scenario_list) {

  cat(paste("Scenario:",scen,"\n"), file = logfile_results, append = TRUE)

  design_pred_scaled_cont <- scale(design_pred[[scen]][ , input_cont_list],
                                   center = inputs_centre,
                                   scale = inputs_scale )
  design_pred_scaled <- as.data.frame( design_pred[[scen]]  )
  design_pred_scaled[ , input_cont_list] <- design_pred_scaled_cont
  myem[[scen]] <- emulator_predict( design_pred_scaled )

}

#' ### Cap glacier mean projections
# GLACIER CAP: MEAN PROJECTIONS
for (scen in scenario_list) {
  if (i_s == "GLA" &&
      max( myem[[scen]]$mean ) > max_glaciers[[reg]] ) {
    cat( sprintf("\nCapping %s mean %s projections at %.3f cm SLE\n",
                 reg, scen, max_glaciers[[reg]]), file = logfile_results, append = TRUE)
    cat( sprintf("Initial range: %.3f - %.3f cm SLE\n", min( myem[[scen]]$mean ),
                 max( myem[[scen]]$mean )), file = logfile_results, append = TRUE)
    myem[[scen]]$mean[ myem[[scen]]$mean > max_glaciers[[reg]] ] <- max_glaciers[[reg]]
    cat( sprintf("Final range: %.3f - %.3f cm SLE\n", min( myem[[scen]]$mean ),
                 max( myem[[scen]]$mean)), file = logfile_results, append = TRUE)
  }
}

#' ## Add emulator uncertainty
# Add emulator uncertainty to projections
projections <- list()

for (scen in scenario_list) {

  # Initialise
  projections[[scen]] <- matrix( nrow = N_temp, ncol = N_ts)
  colnames(projections[[scen]]) <- paste0("y", years_em)

  # For each GSAT projection
  for (ss in 1:N_temp){

    # FACTS: if mvtnorm package is a pain, can use another
    # VARIANCE MATRIX: run type = "var" for predict. Can get quite wiggly!
    # xxx replace with multivariate t?
    projections[[ scen ]][ss, ] <- mvtnorm::rmvnorm(n = 1, mean = myem[[scen]]$mean[ss, ], sigma = myem[[scen]]$var[ss, , ])

  }

  # GLACIER CAP: FINAL PROJECTIONS
  if (i_s == "GLA" &&
      max( projections[[ scen ]] ) > max_glaciers[[reg]] ) {
    cat(sprintf("\nCapping %s final %s projections at %.3f cm SLE\n", reg, scen,
                max_glaciers[[reg]]), file = logfile_results, append = TRUE)

    cat( sprintf("Initial range: %.3f - %.3f cm SLE\n",
                 min( projections[[ scen ]]  ),
                 max( projections[[ scen ]] )), file = logfile_results, append = TRUE)

    projections[[ scen ]][ projections[[ scen ]] > max_glaciers[[reg]] ] <- max_glaciers[[reg]]
    cat( sprintf("Final range: %.3f - %.3f cm SLE\n", min( projections[[ scen ]]  ),
                 max( projections[[ scen ]]  )), file = logfile_results, append = TRUE)
  }

}

#' ### Quantiles

projections_quant <- list()

for (scen in scenario_list) {

  # Projections
  projections_quant[[scen]] <- matrix( nrow = length(q_list), ncol = N_ts)
  colnames(projections_quant[[scen]]) <- paste0("y", years_em)
  rownames(projections_quant[[scen]]) <- paste0("q", q_list)
  for (yy in years_em) {
    projections_quant[[scen]][ , paste0("y", yy) ] <- quantile(projections[[scen]][, paste0("y", yy)], q_list)
  }

}


# PRINT UNCALIBRATED PROJECTIONS TO CSV FILES
write_outputs(write_mean)


#' ### Write summary results to text file

# PRINT SUMMARY
cat("\n_________________________________________\n", file = logfile_results, append = TRUE)
cat("PROJECTIONS: uncalibrated\n\n", file = logfile_results, append = TRUE)

for (yy in c(2100, 2300)) {

  if (yy %in% years_em) {

    cat(paste0("\n",yy,"\n"), file = logfile_results, append = TRUE)

    for (scen in scenario_list) {

      # Mean
      cat ("Mean:\n",  file = logfile_results, append = TRUE)
      cat(sprintf("%s: %.1f [%.1f,%.1f] cm SLE\n",
                  scen,
                  quantile(myem[[scen]]$mean[ , paste0("y", yy) ], probs = 0.5),
                  quantile(myem[[scen]]$mean[ , paste0("y", yy) ], probs = 0.05),
                  quantile(myem[[scen]]$mean[ , paste0("y", yy) ], probs = 0.95)),
          file = logfile_results, append = TRUE)

      # Final
      cat ("Final:\n",  file = logfile_results, append = TRUE)

      cat(sprintf("%s: %.1f [%.1f,%.1f] cm SLE\n",
                  scen,
                  projections_quant[[scen]][ q_list == 0.5, paste0("y", yy) ],
                  projections_quant[[scen]][ q_list == 0.05, paste0("y", yy) ],
                  projections_quant[[scen]][ q_list == 0.95, paste0("y", yy) ]),
          file = logfile_results, append = TRUE)
    }
  }
}



#' # Calibrate
# Calibrate --------------------------------------------------------------------

#' ## Model-obs differences

# History matching

# xxx Make this multivariate! and rename because confusing
obs_change <- obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
obs_change_err <- total_err[obs_data$Year == cal_end]

cat(paste0("\nObserved change (", cal_start,"-", cal_end, "): "), file = logfile_results, append = TRUE)
cat(sprintf("%.3f +/- %.3f cm SLE (1 sigma obs error)\n\n", obs_change, obs_data[obs_data$Year == cal_end,"SLE_sd"]), file = logfile_results, append = TRUE)
cat(sprintf("     +/- %.3f cm SLE (3 sigma total error)\n\n", 3*obs_change_err), file = logfile_results, append = TRUE)

# Calculate difference between each ensemble member (mean and final) and observations
dist_mean <- list()
dist_proj <- list()

cat("_________________________\n", file = logfile_results, append = TRUE)
cat("CALIBRATION\n", file = logfile_results, append = TRUE)

# xxx Note by not subtracting cal_start we are assuming already baselined to this year!
cat("\nCalculating difference between ensemble members and observations\n",
    file = logfile_results, append = TRUE)
for (scen in scenario_list) {
  dist_mean[[scen]] <- myem[[scen]]$mean[, paste0("y",cal_end) ] - obs_change
  dist_proj[[scen]] <- projections[[scen]][, paste0("y",cal_end) ] - obs_change
}

#' ## History matching

# History matching calibration for mean and final projections
cat("\nHistory matching calibration\n", file = logfile_results, append = TRUE)

# Save NROY projections
myem_nroy <- list()
proj_nroy <- list()

for (scen in scenario_list) {
  #cat(paste0("\n", scen,"\n"), file = logfile_results, append = TRUE)
  cat("Calibrating mean projections:\n", file = logfile_results, append = TRUE)
  myem_nroy[[scen]] <- do_calibration(dist_mean[[scen]], "history_matching")
  cat("Calibrating full projections:\n", file = logfile_results, append = TRUE)
  proj_nroy[[scen]] <- do_calibration(dist_proj[[scen]], "history_matching")
}


#' ### Quantiles
projections_nroy_quant <- list()

for (scen in scenario_list) {

  # Calibrated projections
  projections_nroy_quant[[scen]] <- matrix( nrow = length(q_list), ncol = N_ts)
  colnames(projections_nroy_quant[[scen]]) <- paste0("y", years_em)
  rownames(projections_nroy_quant[[scen]]) <- paste0("q", q_list)
  for (yy in years_em) {
    projections_nroy_quant[[scen]][ , paste0("y", yy) ] <-
      quantile(projections[[scen]][proj_nroy[[scen]], paste0("y", yy)], q_list)
  }

}

# PRINT CALIBRATED PROJECTIONS TO CSV FILES
# xxx add calibrated option to this function!
# write_outputs(write_mean)


#' ### Write results
#'
#' # PRINT SUMMARY TO SCREEN
cat("_______________________________________\n", file = logfile_results, append = TRUE)
cat("PROJECTIONS: calibrated (history matching)\n", file = logfile_results, append = TRUE)


# PRINT SUMMARY TO SCREEN
for (yy in c(2100, 2300)) {

  if (yy %in% years_em) {

    cat(paste0("\n", yy,"\n"), file = logfile_results, append = TRUE)

    for (scen in scenario_list) {

      # Mean
      # cat(sprintf("NROY: %.1f [%.1f,%.1f] cm SLE",
      #                quantile(myem[[scen]]$mean[ myem_nroy[[scen]] , "y2100"], probs = 0.5),
      #                quantile(myem[[scen]]$mean[ myem_nroy[[scen]] , "y2100"], probs = 0.05),
      #                quantile(myem[[scen]]$mean[ myem_nroy[[scen]] , "y2100"], probs = 0.95)))

      # Final
      cat(sprintf("%s: %.1f [%.1f,%.1f] cm SLE\n",
                  scen,
                  projections_nroy_quant[[scen]][ q_list == 0.5, paste0("y",yy) ],
                  projections_nroy_quant[[scen]][ q_list == 0.05, paste0("y",yy) ],
                  projections_nroy_quant[[scen]][ q_list == 0.95, paste0("y",yy) ]),
          file = logfile_results, append = TRUE)
    }
  }
}


#' # PRINT SUMMARY TO SCREEN
cat("_______________________________________\n", file = logfile_results, append = TRUE)
cat("PROJECTIONS: calibrated (Bayesian)\n", file = logfile_results, append = TRUE)

#' ## Bayesian calibration
cat("\nBayesian calibration\n", file = logfile_results, append = TRUE)

# Save normalised weights
myem_weights <- list()
proj_weights <- list()
for (scen in scenario_list) {
  myem_weights[[scen]] <- do_calibration(dist_mean[[scen]], "Bayesian")
  proj_weights[[scen]] <- do_calibration(dist_proj[[scen]], "Bayesian")
}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END OF FACTS ANALYSIS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PLOTS FOR PUBLICATION BUT NOT NEEDED IN FACTS
do_plots <- TRUE

# xxx Use obs_change and obs_change_err in plot_figures; implaus_thresh = 3 - done?

if (do_plots) {
  # w <- seq(1,1000)
  # v <- sort(runif(1000))
  # AR6_rgb_med[["SSP585"]] <- rgb(132, 11, 34, maxColorValue = 255, alpha = 153) # SSP5-85

  #' ### Plot results
  cat("\nPlot uncalibrated projections:\n",file = logfile_results, append = TRUE)

  if (plot_level > 0) {
    pdf( file = paste0( outdir, "/", out_name, "_UNCALIBRATED.pdf"),
         width = 9, height = 5)
    plot_designs("prior", plot_level)
    plot_timeseries("prior", plot_level)
    plot_scatter("prior", plot_level)
    plot_distributions("prior", plot_level)
    dev.off()
  }

  cat("\nPlot calibrated projections:\n", file = logfile_results, append = TRUE)

  if (plot_level > 0) {
    pdf( file = paste0( outdir, "/", out_name, "_CALIBRATED.pdf"),
         width = 9, height = 5)
    plot_designs("posterior", plot_level)
    plot_timeseries("posterior", plot_level)
    plot_scatter("posterior", plot_level)
    plot_distributions("posterior", plot_level)
    plot_bayesian()
    dev.off()
  }

}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Save workspace with emulator and results together
save.image( paste0(outdir, "/", out_name, "_RESULTS.RData") )

cat("...done.\n")










