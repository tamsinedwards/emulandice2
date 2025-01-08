#' ---
#' title: "emulandice2: predict"
#' output:
#'    html_notebook:
#'      toc: true
#'      number_sections: true
#' ---
#' MULTIVARIATE EMULATION OF LAND ICE CONTRIBUTIONS TO SEA LEVEL
# Tamsin Edwards
# Originally based on emulandice (Edwards et al., 2021)
# Updated to use multivariate code by Jonathan Rougier
#
# Emulators are built by emulator_build.R using multi-model simulations of
# Greenland, Antarctica, and glacier regions (19) from EU H2020 PROTECT project.
# These simulations start between 1950 and 2005 and end between 2100-2300.
#
# Emulator projections here (main.R) usually begin in 1995, 2000 or 2005 ('cal_start')
# and end between 2100 and 2300 ('final_year').
#
#_______________________________________________________________________________

#' # Get FACTS args
# Get FACTS args ------------------------------------------------------------

cat("_______________________________________\n")
cat("Hello! Welcome to emulandice2: predict\n")
cat("_______________________________________\n")
cat("Requested settings:\n")

# Get arguments from RScript command in emulandice_steer.sh
args <- commandArgs(TRUE)
if (length(args) == 0) {

  # Defaults if no args set (used for testing and Markdown)
  cat("NOTE: No arguments set - using defaults\n")
  i_s <- "GIS"
  reg <- "ALL" #"RGI03"

  # CLIMATE DATA FILE: constructed from filename and package data directory
  climate_data_file <- "../gsat/twolayer_SSPs.h5" # assumes running locally if testing

  # EMULATOR BUILD FILE: constructed from the above settings
  # Directory has to match rdatadir in the build file it is loading
  emu_file <- "./data-raw/GIS_ALL_CISM_pow_exp_01_EMULATOR.RData"

  outdir_facts <- "./out/"

  seed <- 2024 # Same random seed as emulator_build.R
  pipeline_id <- format(Sys.time(), "%y%m%d_%H%M%S") # YYMMDD_HHMMSS

} else {

  i_s <- args[1] # ice_source
  reg <- args[2] # region
  emu_file <- args[3] # emulator build file with path
  climate_data_file <- args[4] # climate netcdf with path
  facts_ssp <- args[5] # ssp
  outdir_facts <- args[6] # output directory
  seed <- args[7] # random seed
  pipeline_id <- args[8]

}

cat(sprintf("Ice source: %s\n", i_s))
stopifnot(i_s %in% c("GIS", "AIS", "GLA"))

# Region of ice source
cat(sprintf("Region: %s\n", reg))
stopifnot(reg %in% c("ALL", paste0("RGI", sprintf("%02i",1:19))))

# Emulator build file name includes ice model_list and emulator_settings
cat(sprintf("Emulator build file: %s\n", emu_file))

# Netcdf name
cat(sprintf("Climate file: %s\n", climate_data_file))
stopifnot(file.exists(climate_data_file))

# SSP (could extract from filename)
cat(sprintf("Scenario: %s\n", facts_ssp))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# OTHER SETTINGS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat(sprintf("Outputs will be placed in directory: %s\n", outdir_facts))

# Write projection CSV files
write_csv <- FALSE
cat(sprintf("Write projections CSV files: %s\n", write_csv))

# Netcdf filename
# Don't change because we use format to substitute reg with regional for ice sheet files
ncname <- paste0(outdir_facts, pipeline_id,"_",reg,"_globalsl.nc")
cat(sprintf("Projections netcdf filename will be: %s\n", ncname))

#' # Setup
# Setup ------------------------------------------------------------


# BASIC CONSISTENCY CHECKS

# Check emulator build arguments are consistent
if ( ! grepl(i_s, basename(emu_file)) ) {
  stop("Requested ice source is not found in emulator build file name" )
}
if ( ! grepl(reg, basename(emu_file)) ) {
  stop("Requested ice source region is not found in emulator build file name" )
}


# LOAD EMULATOR AND OTHER STUFF
cat("\nLoading emulator build file\n")
stopifnot(file.exists(emu_file))
load( file = emu_file)


#___________________________________________
# RESET SOME SETTINGS FOR FACTS

# Various code uses scenario loops
# so design_pred, myem, projections and projections_quant are set up as lists
# with label [[scen]]
# FACTS uses only one scenario at a time but will keep these loops for now:
# partly for plot scripts also used by emulator_build.R (multiple scenarios),
# partly in case I can use for plotting multiple scenarios later
scenario_list <- paste0("SSP",substring(facts_ssp,4)) # emulandice expects upper case

set.seed(seed)

# Plots: 0 = none, 1 = main, 2 = nearly all, 3 = replot SIMS.pdf with model error
plot_level <- 2

# Number of 2LM projections of GSAT expected per SSP
# (and therefore total number of samples for book-keeping by GSAT value)
# Checks when reading in netcdf - could get rid of this
#N_2LM <- 50L # 2237L for AR6 files


cat("Running...\n")

# Log file from emulator_build.R is same name but _build.txt
if ( ! file.exists(outdir_facts) ) {
  cat(sprintf("Creating output directory: %s\n", outdir_facts))
  dir.create(file.path(outdir_facts))
}

logfile_results <- paste0(outdir_facts, out_name,"_results.txt")
cat(sprintf("\nemulandice2: %s %s\n\n", i_s, reg), file = logfile_results)

cat(sprintf("\nLoaded emulator file: %s\n", emu_file), file = logfile_results, append = TRUE)

# Sample size for calibration prior distributions
N_prior <- 500L # XXX For testing; later make this e.g.10000L

# Quantiles to output
q_list <- c( 0.50, 0.05, 0.95, 0.17, 0.83, 0.25, 0.75 )

#' # Priors are default or custom
# Priors -----------------------------------------------------------------------

# Uniform is currently uniform for all except GIS retreat
# Custom currently restricts ranges of ice sheet resolution, AIS ISMIP6 heat fluxes,
# and GLA temp_bias and precip_corr_factor
if (i_s %in% c("GIS", "AIS")) { prior_choices <- "custom"
} else prior_choices <- "uniform"
stopifnot(prior_choices %in% c("uniform", "custom"))
cat("Prior choices:", prior_choices,"\n",  file = logfile_results, append = TRUE)

#' # Set model discrepancy
# Model error -----------------------------------------------------------------------

# Model discrepancy
# xxx Using multiple of obs error for now
if (i_s == "GLA") { scale_mod_err = 20
} else scale_mod_err = 3
stopifnot( scale_mod_err > 1 )
model_err <- scale_mod_err * obs_data[,"SLE_sd"]

cat(paste("\nModel error for calibration: using",scale_mod_err,"x obs error", "\n"),
    file = logfile_results, append = TRUE)

# Calculate combined discrepancy
total_err <- sqrt(obs_data[,"SLE_sd"]^2 + model_err^2)

#' # Option to replot simulations
# Replot sims -----------------------------------------------------------------------

# This time the history matching window will include model discrepancy (total_err)
if (plot_level > 2) {
  pdf( file = paste0( outdir, out_name, "_SIMS.pdf"),
       width = 9, height = 5)
  emulandice2::plot_designs("sims", plot_level)
  emulandice2::plot_timeseries("sims", plot_level)
  emulandice2::plot_scatter("sims", "none", plot_level)
  emulandice2::plot_distributions("sims", plot_level)
  dev.off()
}

#' # Design: calibration
# Design: calibration -----------------------------------------------------------------------

# This overwrites uniform design_pred from RData build file (as intended, for reusing plot scripts)
# design_pred <- load_design_to_pred( "AR6_2LM" ) # XXXX

# Fixed GSAT design for calibrating ice model parameters
design_fixed <- load_design_to_pred( "fixed_temp", N_prior ) # Large N, 3 fixed GSAT

# Grab names (i.e. these are set in design function rather than passed to it)
fixed_temp_list <- names(design_fixed)

# Store number of samples per scenario
# Used for outputs
# N_temp <- length( design_pred[[1]][ , 1] ) XXXX check if used before end

#' # Predict for calibration
# Predict for calibration ------------------------------------------------------


cat("\nPredict:\n", file = logfile_results, append = TRUE)

# Reinitialise to delete the previous uniform and main effects projections
# from emulator_build.R (reusing scenario-based plotting code)
myem <- list()

# emulator_predict() calls emu_mv with type = "var"
# using emulator object saved to RData workspace file
# Here scen is fixed_temp not a scenario, but keep label for consistency
for (scen in fixed_temp_list) {

  cat(paste("Fixed climate:",scen,"\n"), file = logfile_results, append = TRUE)

  # Rescale priors using same scaling factors as for simulator inputs
  design_fixed_scaled_cont <- scale(design_fixed[[scen]][ , input_cont_list],
                                    center = inputs_centre,
                                    scale = inputs_scale )
  design_fixed_scaled <- as.data.frame( design_fixed[[scen]]  )
  design_fixed_scaled[ , input_cont_list] <- design_fixed_scaled_cont

  myem[[scen]] <- emulator_predict( design_fixed_scaled )

}

#' # Sample from emulator uncertainty
projections <- list()

for (scen in fixed_temp_list) {

  # Generate projections by sampling mean + uncertainty
  projections[[scen]] <- emulator_uncertainty(myem[[scen]])

}

#' # Calculate likelihoods
# Calculate likelihoods--------------------------------------------------------------------

cat("_________________________\n", file = logfile_results, append = TRUE)
cat("LIKELIHOODS AND WEIGHTS\n", file = logfile_results, append = TRUE)

#' ## Calculate model-obs differences

# xxx Make this multivariate! and rename because confusing
obs_change <- obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
obs_change_err <- total_err[obs_data$Year == cal_end]

cat(paste0("\nObserved change (", cal_start,"-", cal_end, "):\n"), file = logfile_results, append = TRUE)
cat(sprintf("%.3f +/- %.3f cm SLE (3 sigma total error)\n\n", obs_change, 3*obs_change_err), file = logfile_results, append = TRUE)

# Calculate difference between each ensemble member (mean and final) and observations
dist_mean <- list()
dist_proj <- list()

# xxx Note by not subtracting cal_start we are assuming already baselined to this year!
cat("Calculating difference between ensemble members and observations\n",
    file = logfile_results, append = TRUE)
for (scen in fixed_temp_list) {
  dist_mean[[scen]] <- myem[[scen]]$mean[, paste0("y",cal_end) ] - obs_change
  dist_proj[[scen]] <- projections[[scen]][, paste0("y",cal_end) ] - obs_change
}

# Save normalised weights
myem_weights <- list()
proj_weights <- list()
for (scen in fixed_temp_list) {
  myem_weights[[scen]] <- do_calibration(dist_mean[[scen]])
  proj_weights[[scen]] <- do_calibration(dist_proj[[scen]])
}

cat("_________________________\n", file = logfile_results, append = TRUE)
cat("LOAD DESIGNS\n", file = logfile_results, append = TRUE)

# Note the posterior is not a direct weighting of the full prior {ice inputs, GSAT}
# Both are generated separately, so GSAT is not calibrated:
#
# (a) Prior = samples from ice model priors exactly once for each FaIR GSAT simulation
# this is done in load_design_to_pred()
# (b) Posterior = samples from ice model posteriors (weighted priors), bolts on FaIR prior
# (i.e. because GSAT is intentionally not calibrated)
# this is done here, using previous output from load_design_to_pred() call [design_fixed]

# Note these are also resampled for each SSP, introducing more random noise

# I'm sure there is a neater and more efficient way to do all this xxx

#' # Design: priors
# Design: priors -----------------------------------------------------------------------

cat("Design: priors\n", file = logfile_results, append = TRUE)
print("Design: priors")

# Design "AR6_2LM" uses 2-layer model (FaIR) GSAT projections for SSPs
# i.e. climate_data_file from FACTS is used in this function

# Prior design: ice model input priors plus FaIR GSAT prior for each SSP
# Independently samples from ice model priors, once for each FaIR trajectory
design_prior <- load_design_to_pred( "AR6_2LM" )

# Get number of FaIR samples per scenario
N_temp <- length( design_prior[[1]][ , 1] )

#' # Design: posteriors
# Design: posteriors -----------------------------------------------------------------------

cat("Design: posteriors (only ice model inputs are calibrated, not climate)\n", file = logfile_results, append = TRUE)
print("Design: posterior")

# Posterior design: sample ice model input posteriors
# i.e. from large priors using weights from Bayesian calibration
# Exclude GSAT column(s) when doing this
design_inputs_post <- apply(design_fixed[["mean_temp"]][ , -c(1:length(temps_list))], 2, function(x) {
  sample(x, N_temp, replace = TRUE, prob = proj_weights[["mean_temp"]])
})

# Add GSAT prior column(s)
design_pred <- list()
for (scen in scenario_list) { # mimic scenario list, but should probably delete this everywhere
  design_pred[[scen]] <- cbind( design_prior[[scen]][ , temps_list_names ], design_inputs_post)
  colnames( design_pred[[scen]])[1:length(temps_list)] <- temps_list_names
}


#' # Predict: prior
# Predict: prior --------------------------------------------------------------------

cat("\nPredict: uncalibrated\n", file = logfile_results, append = TRUE)
print("Predict: prior")

# Adds projection for SSP to list with the previous fixed temp sample
# (note different dims because N_samp > N_temp)
for (scen in scenario_list) {

  cat(paste("Scenario:",scen,"\n"), file = logfile_results, append = TRUE)

  # Prior design
  design_prior_scaled_cont <- scale(design_prior[[scen]][ , input_cont_list],
                                    center = inputs_centre,
                                    scale = inputs_scale )
  design_prior_scaled <- as.data.frame( design_prior[[scen]]  )
  design_prior_scaled[ , input_cont_list] <- design_prior_scaled_cont

  # Projections: PRIOR MEAN
  # Returns $mean, $sd, $var
  myem[[scen]] <- emulator_predict( design_prior_scaled )

}

#' ## Cap glacier mean prior projections
for (scen in scenario_list) {
  if (i_s == "GLA" &&
      max( myem[[scen]]$mean ) > glacier_cap ) {
    cat( sprintf("\nCapping %s mean %s projections at %.3f cm SLE\n",
                 reg, scen, glacier_cap), file = logfile_results, append = TRUE)
    cat( sprintf("Initial range: %.3f - %.3f cm SLE\n", min( myem[[scen]]$mean ),
                 max( myem[[scen]]$mean )), file = logfile_results, append = TRUE)

    myem[[scen]]$mean <- emulandice2::weight_glacier_cap( myem[[scen]]$mean )

    cat( sprintf("Final range: %.3f - %.3f cm SLE\n", min( myem[[scen]]$mean ),
                 max( myem[[scen]]$mean)), file = logfile_results, append = TRUE)
  }
}

#' # Predict: emulator final projections with uncertainties
# Again add emulator projections for scenario  to projections list

for (scen in scenario_list) {

  # Projections: PRIOR FINAL
  # Returns sample of final projections by sampling emulator $mean and $var
  projections[[scen]] <- emulator_uncertainty(myem[[scen]])

  #' ## Cap glacier final prior projections
  if (i_s == "GLA" &&
      max( projections[[ scen ]] ) > glacier_cap ) {
    cat(sprintf("\nCapping %s final %s projections at %.3f cm SLE\n", reg, scen,
                glacier_cap), file = logfile_results, append = TRUE)

    cat( sprintf("Initial range: %.3f - %.3f cm SLE\n",
                 min( projections[[ scen ]]  ),
                 max( projections[[ scen ]] )), file = logfile_results, append = TRUE)

    projections[[ scen ]] <- emulandice2::weight_glacier_cap(projections[[ scen ]])

    cat( sprintf("Final range: %.3f - %.3f cm SLE\n", min( projections[[ scen ]]  ),
                 max( projections[[ scen ]]  )), file = logfile_results, append = TRUE)
  }

}


#' # Predict: posterior
# Predict: posterior --------------------------------------------------------------------

cat("\nPredict: calibrated\n", file = logfile_results, append = TRUE)
print("Predict: posterior")

# Posterior mean
# Projections using posterior distributions for inputs
myem_post <- list() # xxx to mirror myem - rename everything later

for (scen in scenario_list) {

  cat(paste("Scenario:",scen,"\n"), file = logfile_results, append = TRUE)

  # Posterior design
  design_pred_scaled_cont <- scale(design_pred[[scen]][ , input_cont_list],
                                   center = inputs_centre,
                                   scale = inputs_scale )
  design_pred_scaled <- as.data.frame( design_pred[[scen]]  )
  design_pred_scaled[ , input_cont_list] <- design_pred_scaled_cont

  # Projections: POSTERIOR MEAN
  # Returns $mean, $sd, $var
  myem_post[[scen]] <- emulator_predict( design_pred_scaled )

}

#' ## Cap glacier mean posterior projections
for (scen in scenario_list) {
  if (i_s == "GLA" &&
      max( myem_post[[scen]]$mean ) > glacier_cap ) {
    cat( sprintf("\nCapping %s mean %s projections at %.3f cm SLE\n",
                 reg, scen, glacier_cap), file = logfile_results, append = TRUE)
    cat( sprintf("Initial range: %.3f - %.3f cm SLE\n", min( myem_post[[scen]]$mean ),
                 max( myem_post[[scen]]$mean )), file = logfile_results, append = TRUE)

    # XXX Currently applies hard cap i.e. old method
    myem_post[[scen]]$mean <- emulandice2::weight_glacier_cap( myem_post[[scen]]$mean )

    cat( sprintf("Final range: %.3f - %.3f cm SLE\n", min( myem_post[[scen]]$mean ),
                 max( myem_post[[scen]]$mean )), file = logfile_results, append = TRUE)
  }
}

# Posterior sample with uncertainties
proj_post <- list()

for (scen in scenario_list) {

  # Projections: POSTERIOR FINAL
  # Returns sample of final projections by sampling emulator $mean and $var
  proj_post[[scen]] <- emulator_uncertainty(myem_post[[scen]])

  #' ## Cap glacier final posterior projections
  if (i_s == "GLA" &&
      max( proj_post[[ scen ]] ) > glacier_cap ) {
    cat(sprintf("\nCapping %s final %s projections at %.3f cm SLE\n", reg, scen,
                glacier_cap), file = logfile_results, append = TRUE)

    cat( sprintf("Initial range: %.3f - %.3f cm SLE\n",
                 min( proj_post[[ scen ]]  ),
                 max( proj_post[[ scen ]] )), file = logfile_results, append = TRUE)

    # XXX Currently applies hard cap i.e. old method
    proj_post[[ scen ]] <- emulandice2::weight_glacier_cap(proj_post[[ scen ]])

    cat( sprintf("Final range: %.3f - %.3f cm SLE\n", min( proj_post[[ scen ]]  ),
                 max( proj_post[[ scen ]]  )), file = logfile_results, append = TRUE)
  }

}

#_______________________________________________________________________________
#' # Quantiles: prior
# Quantiles: prior --------------------------------------------------------------------

#' ## Calculate prior (uncalibrated) quantiles
projections_quant <- list()

for (scen in scenario_list) {

  # Setup
  projections_quant[[scen]] <- matrix( nrow = length(q_list), ncol = N_ts)
  colnames(projections_quant[[scen]]) <- paste0("y", years_em)
  rownames(projections_quant[[scen]]) <- paste0("q", q_list)

  # Quantiles
  for (yy in years_em) {
    projections_quant[[scen]][ , paste0("y", yy) ] <- quantile(projections[[scen]][, paste0("y", yy)], q_list)
  }

}

#' # Quantiles: posterior
# Quantiles: posterior --------------------------------------------------------------------

#' ## Calculate posterior (calibrated) quantiles
proj_post_quant <- list()

for (scen in scenario_list) {

  # Setup as before
  proj_post_quant[[scen]] <- matrix( nrow = length(q_list), ncol = N_ts)
  colnames(proj_post_quant[[scen]]) <- paste0("y", years_em)
  rownames(proj_post_quant[[scen]]) <- paste0("q", q_list)

  # Quantiles
  for (yy in years_em) {
    proj_post_quant[[scen]][ , paste0("y", yy) ] <- quantile(proj_post[[scen]][, paste0("y", yy)], q_list)
  }

}

#' # Print results to file
# Print results to file --------------------------------------------------------------------

# PRINT SUMMARY
cat("\n\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n", file = logfile_results, append = TRUE)
cat("RESULTS: 50 [17, 83]% percentiles (cm SLE)\n", file = logfile_results, append = TRUE)
cat("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n", file = logfile_results, append = TRUE)

cat("\n_________________________________________\n", file = logfile_results, append = TRUE)
cat("PROJECTIONS: uncalibrated\n\n", file = logfile_results, append = TRUE)

yy_table <- c(2100, 2150, 2200, 2300)
yy_table <- yy_table[ yy_table %in% years_em ]

# Quantile range to report
qr <- c(0.17,0.83)
stopifnot(qr %in% q_list)

# Write mean table
cat(paste("\t", paste(yy_table,collapse = "\t"),"\n"), file = logfile_results, append = TRUE)
cat(paste("Mean\n"), file = logfile_results, append = TRUE)
for (scen in scenario_list) {
  for (yy in yy_table) {
    if (yy == yy_table[1]) first_el <- scen
    else first_el <- table_row
    table_row <- sprintf("%s\t%.1f [%.1f, %.1f]", first_el,
                         quantile(myem[[scen]]$mean[ , paste0("y", yy) ], probs = 0.5),
                         quantile(myem[[scen]]$mean[ , paste0("y", yy) ], probs = qr[1]),
                         quantile(myem[[scen]]$mean[ , paste0("y", yy) ], probs = qr[2]))
  }

  table_row <- paste(table_row, "\n")
  cat(table_row, file = logfile_results, append = TRUE)

}

# Write final table
cat(paste("Final\n"), file = logfile_results, append = TRUE)

for (scen in scenario_list) {
  for (yy in yy_table) {
    if (yy == yy_table[1]) {
      table_row <- sprintf("%s: %.1f [%.1f, %.1f]",scen,
                           projections_quant[[scen]][ q_list == 0.5, paste0("y", yy) ],
                           projections_quant[[scen]][ q_list == qr[1], paste0("y", yy) ],
                           projections_quant[[scen]][ q_list == qr[2], paste0("y", yy) ])
    } else {
      table_row <- sprintf("%s\t%.1f [%.1f, %.1f]\t", table_row,
                           projections_quant[[scen]][ q_list == 0.5, paste0("y", yy) ],
                           projections_quant[[scen]][ q_list == qr[1], paste0("y", yy) ],
                           projections_quant[[scen]][ q_list == qr[2], paste0("y", yy) ])
    }
  }

  table_row <- paste(table_row, "\n")
  cat(table_row, file = logfile_results, append = TRUE)

}

cat("\n_________________________________________\n", file = logfile_results, append = TRUE)
cat("PROJECTIONS: calibrated\n\n", file = logfile_results, append = TRUE)

# Write mean table
cat(paste("\t", paste(yy_table,collapse = "\t"),"\n"), file = logfile_results, append = TRUE)
cat(paste("Mean\n"), file = logfile_results, append = TRUE)
for (scen in scenario_list) {
  for (yy in yy_table) {
    if (yy == yy_table[1]) first_el <- scen
    else first_el <- table_row
    table_row <- sprintf("%s\t%.1f [%.1f, %.1f]", first_el,
                         quantile(myem_post[[scen]]$mean[ , paste0("y", yy) ], probs = 0.5),
                         quantile(myem_post[[scen]]$mean[ , paste0("y", yy) ], probs = qr[1]),
                         quantile(myem_post[[scen]]$mean[ , paste0("y", yy) ], probs = qr[2]))
  }

  table_row <- paste(table_row, "\n")
  cat(table_row, file = logfile_results, append = TRUE)

}

# Write final table
cat(paste("Final\n"), file = logfile_results, append = TRUE)

for (scen in scenario_list) {
  for (yy in yy_table) {
    if (yy == yy_table[1]) {
      table_row <- sprintf("%s: %.1f [%.1f, %.1f]",scen,
                           proj_post_quant[[scen]][ q_list == 0.5, paste0("y", yy) ],
                           proj_post_quant[[scen]][ q_list == qr[1], paste0("y", yy) ],
                           proj_post_quant[[scen]][ q_list == qr[2], paste0("y", yy) ])
    } else {
      table_row <- sprintf("%s\t%.1f [%.1f, %.1f]\t", table_row,
                           proj_post_quant[[scen]][ q_list == 0.5, paste0("y", yy) ],
                           proj_post_quant[[scen]][ q_list == qr[1], paste0("y", yy) ],
                           proj_post_quant[[scen]][ q_list == qr[2], paste0("y", yy) ])
    }
  }

  table_row <- paste(table_row, "\n")
  cat(table_row, file = logfile_results, append = TRUE)

}

cat("\n\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n", file = logfile_results, append = TRUE)
cat("END OF RESULTS\n", file = logfile_results, append = TRUE)
cat("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\n", file = logfile_results, append = TRUE)

#' # Write other outputs
# Write other outputs --------------------------------------------------------------------

# WRITE PROJECTIONS TO CSV AND NETCDF FILES
write_outputs(write_csv)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END OF FACTS ANALYSIS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Save workspace now in case plots fail
save.image( paste0(outdir_facts, out_name, "_RESULTS.RData") )

if (plot_level > 0) {

  #' ### Plot results if requested

  cat("\nPlot uncalibrated projections\n",file = logfile_results, append = TRUE)

  pdf( file = paste0( outdir_facts, out_name, "_UNCALIBRATED.pdf"),
       width = 9, height = 5)
  plot_designs("prior", plot_level)
  plot_timeseries("prior", plot_level)
  plot_scatter("prior", "AR6_2LM", plot_level)
  plot_distributions("prior", plot_level)
  dev.off()

  cat("Plot calibrated projections\n", file = logfile_results, append = TRUE)
  pdf( file = paste0( outdir_facts, out_name, "_CALIBRATED.pdf"),
       width = 9, height = 5)
  plot_designs("posterior", plot_level)
  # Note no posterior time series plots
  plot_scatter("posterior", "AR6_2LM", plot_level)
  plot_distributions("posterior", plot_level)
  plot_bayesian()
  dev.off()
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


cat("...done.\n")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Misc notes
# xxx Use obs_change and obs_change_err in plot_figures; implaus_thresh = 3 - done?

# w <- seq(1,1000)
# v <- sort(runif(1000))
# AR6_rgb_med[["SSP585"]] <- rgb(132, 11, 34, maxColorValue = 255, alpha = 153) # SSP5-85








