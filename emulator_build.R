#' ---
#' title: "emulandice2: emulator_build"
#' output:
#'    html_notebook:
#'      toc: true
#'      number_sections: true
#'
#' ---
#_______________________________________________________________________________
# BUILD EMULATOR
#
# Writes RData file: paste0("outdir", out_name, "_EMULATOR.RData")
# to be read by FACTS for predicting land ice contributions with FaIR GSAT projections
#
# Set plot_level > 0 to output plot pdf files
#_______________________________________________________________________________

# SETUP ------------------------------------------------------------------------
#' # SETUP

# Get arguments from RScript
args <- commandArgs(TRUE)

# Defaults if no args set (used for testing and Markdown)
if (length(args) == 0) {

  warning("No arguments set - using defaults")
  i_s <- "GLA"
  reg_num <- 1
  final_year <- 2300

} else {

  # Ice source and final year ----------------------------------------------------------
  #' # Choose ice source and final year

  # Ice source
  i_s <- args[1]

  # Region number (only used by glaciers for now)
  reg_num <- as.numeric(args[2])

  # End year
  final_year <- as.numeric(args[3]) # if past 2100, applies model/ensemble selections later

}

# Fix random seed
set.seed(2024)

# Directory for output files
rdatadir <- "./data-raw/" # RData file containing emulator
outdir <- "./out/" # Everything else

# Create directories if they do not exist (may not if running on HPC)
if ( ! file.exists(rdatadir) ) dir.create(file.path(rdatadir))
if ( ! file.exists(outdir) ) dir.create(file.path(outdir))

# Directories for input datasets
# (all in the same place, but grouped by type in case want to change structure)
inputs_preprocess <- paste0(system.file("extdata", package = "emulandice2"), "/")
inputs_ext <- inputs_preprocess

# Analysis choices ------------------------------------------------------------------------

#' # Analysis choices
#' ## Dataset, ice source, region [ensemble]

# Check ice source name
stopifnot(i_s %in% c("GIS","AIS", "GLA"))

# Later there are options to pick sub-ensembles
ensemble_subset <- NA

# Region is set here

# CHOOSE ICE SHEET SECTOR (when implemented)
if (i_s %in% c("GIS", "AIS")) {
  reg <- "ALL"
  stopifnot(reg %in% c("ALL")) # will add basins
}

# RGI NUMBER
if (i_s == "GLA") reg <- paste0("RGI", sprintf("%02i", reg_num)) # zero-padded

# Check region name is valid
stopifnot(reg %in% c("ALL", paste0("RGI", sprintf("%02i",1:19))))

# ENSEMBLE DATA
# Main end dates of simulations in PROTECT ensembles

# If add new final_year option, need to add to sle_lim list for plots too (xxx add check)
# Currently two Greenland and glacier ensembles to choose from

if (i_s == "AIS") {
  stopifnot(final_year %in% c(2100, 2150, 2200, 2300))
  ensemble_subset <- "all_forced"
  stopifnot( ensemble_subset %in% c("GCM_forced", "RCM_forced", "all_forced")) # only RCM option used for now
}

if (i_s == "GIS") {
  stopifnot(final_year %in% c(2100, 2150, 2200, 2250, 2300))
}

if (i_s == "GLA") {

  ensemble_subset <- "PPE" # xxx Now ignored because ensembles are combined - keeping here for now
  stopifnot(ensemble_subset %in% c("forcing", "PPE"))

  if (ensemble_subset == "forcing" && final_year > 2100) {
    warning("ensemble_subset is set to 'forcing' so reducing final_year to 2100")
    final_year <- 2100
  }
  stopifnot(final_year %in% c(2100, 2300))
}

# Set limit on data size for training GP
target_size <- 1000 # NA to use all simulations

# Long names for outputs
if (i_s == "GIS") ice_name <- "Greenland"  # %in% c("GrIS", "GIS"))
if (i_s == "AIS") ice_name <- "Antarctica"
if (i_s == "GLA") {
  ice_name <- read.csv(paste0(inputs_ext,"/GLA/regionnames.txt"), header = FALSE)[reg_num,1]
}

# Sample size for unif_temps design - used for convenience when adding uncertainty
# (Main effects sample size is set in load_design_to_pred.R, and
# AR6 prior sample is equal to number of GSAT projections)
N_prior <- 2000

# Do LOO validation?
do_loo_validation <- FALSE
N_k <- 10 # for every N_k-th simulation; NA for full LOO

print("***********************************************************************")
print("Hello! Welcome to emulandice2: build")
print("***********************************************************************")

print(paste("Building an emulator for",ice_name,"region",reg,"..."))
if (do_loo_validation) {
  print(paste("LOO with N_k =",N_k,"(could be very slow)"))
}
#' ## Projection times and possible scenarios

# SIMULATION YEARS in dataset i.e. columns in CSV

# First year of simulations we want to use
# checks later this is within CSV file header range
if (i_s == "AIS") first_year <- 1950
if (i_s == "GIS") first_year <- 1960
if (i_s == "GLA") first_year <- 1980

years_sim <- first_year:final_year

# Timeslice frequency to predict (see below: first date is cal_start + nyr)
nyrs = 5

# Check reasonable choice
stopifnot(nyrs %in% c(1, 2, 5, 10))

# Full list of possible emissions scenarios to look for
# (dropped from unif_temps design if not simulated)
scenario_list <- c("SSP119", "SSP126", "SSP245", "SSP370", "SSP534-over", "SSP585")

#' ## Ice model(s)

if (i_s == "AIS") {

  # All models (do not change!)
  model_list_full <- c( "Kori", "PISM", "CISM", "ElmerIce" ) # XXX "BISICLES" )

  # Would drop short simulations anyway but early on is better for emulator inputs
  if (ensemble_subset == "GCM_forced" ||
      (ensemble_subset == "all_forced" && final_year > 2200) ) {
    model_list <- c( "Kori", "PISM" )
  } else model_list <- model_list_full

}

if (i_s == "GIS") {

  # All models (do not change!)
  model_list_full <- c( "CISM", "IMAUICE", "ElmerIce", "GISM" )

  # Pick models to use: full list or CISM only
  if ( final_year <= 2100 ) model_list <- model_list_full
  if ( final_year > 2100 ) model_list <- "CISM"

  # If ElmerIce: change cal range later to 1992-2014 (if 2 yr timeslices)

  # Flag to require matching historical + projection retreat values in select_sims()
  # (only CISM runs do though)
  need_retreat_match <- TRUE

  # Only CISM went beyond 2100 (at all / to any great extent)
  if ( final_year > 2100 &&
       ( length(model_list) > 1 ||
         (length(model_list) == 1 && model_list != "CISM") ) ) return()

}

if (i_s == "GLA") {

  # All models (do not change!)
  model_list_full <- c( "GloGEM", "OGGM") # XXX "GO" )

  # Pick models to use
  model_list <- model_list_full

  # Fraction of glaciers that must have completed (guidance from Fabien Maussion)
  # Currently only applied to OGGM; XXX add GO model to select_sims()
  complete_thresh <- 0.8 # NA to not use

}

# Check selected model names are correct
stopifnot( length( setdiff(model_list, model_list_full )) == 0 )

# Emulator choices ------------------------------------------------------------------------

#' ## Set emulator covariance function
# Choose emulator covariance function here so can put in output name for now

# Currently can choose AR6 settings (mostly linear), matern_5_2, matern_3_2,
# or pow_exp (power-exponential with alpha = 0.1, 1.0, 1.9, 2.0)

# XXX Specify by ice sheet sector later if using

if ( i_s == "AIS") emulator_settings <- "pow_exp_10"

if ( i_s == "GIS") emulator_settings <- "pow_exp_01"

if ( i_s == "GLA") {

  # Default: squared exponential (Gaussian: smooth)
  emulator_settings <- "pow_exp_20"

  # 3rd Jan 2024: Make some large regions more linear if still over-fitting
  if (reg_num %in% c(1, 4, 5, 7, 19)) emulator_settings <- "pow_exp_01"

}
stopifnot(emulator_settings %in% c("IPCC_AR6", "matern_5_2", "matern_3_2",
                                   "pow_exp_01", "pow_exp_10",
                                   "pow_exp_19", "pow_exp_20"))

#' ## Open output file

# Create name for output files
out_name <- paste0(i_s,"_",reg,"_",paste(model_list, collapse = "_"),
                   "_", emulator_settings)
logfile_build <- paste0(outdir, out_name,"_build.txt")

#______________________________________________________
# START WRITING LOG FILE
cat("_____________________________________\n", file = logfile_build)
cat(paste("LAND ICE SOURCE:", ice_name, reg, "\n"), file = logfile_build, append = TRUE)
cat( paste("\nEnsemble subset:", ensemble_subset,"\n"), file = logfile_build, append = TRUE)
cat(paste( "MODELS:", paste(model_list, collapse = ", "), "\n"), file = logfile_build, append = TRUE)
cat(paste("\nDate range of simulations to be used:",
          years_sim[1],"-", years_sim[length(years_sim)], "\n"),
    file = logfile_build, append = TRUE)
cat(paste("\nEmulator covariance:",emulator_settings,"\n"), file = logfile_build, append = TRUE)

#' ## Glacier maximum contributions
# Get glacier cap --------

glacier_cap <- emulandice2::get_glacier_cap(reg)

# Calibration dates --------
#' ## Baseline and calibration dates

# Ice sheets: Otosaka et al. (2023) IMBIE is 1992-2020
# Glaciers: Hugonnet et al. (2021) is 2000-2020
if (i_s == "AIS") cal_end <- 2020
if (i_s == "GIS") cal_end <- 2020 # 2015 # 2014 for 2100 2 yr; but 2015 for ElmerIce or 2300 (or 2020?)
if (i_s == "GLA") cal_end <- 2020 # because OGGM fails if too early xxx obsolete?

# Start of calibration period
# xxx Note cal_start MUST be same as baseline in current code (and makes sense)

# PROTECT: Earliest Greenland = 1960, Antarctic = 1950, glaciers = 1980

# XXX Implement different baselines for 2100/50 and 2300?

# Antarctica
if (i_s == "AIS") cal_start = 2000

# Greenland
if (i_s == "GIS") cal_start = 2000 # xxx change to 1995 when decoupled baseline

# Glaciers: 2000 for most runs, but 2005 for OGGM PPE
if (i_s == "GLA") cal_start = 2000

# Check for current data ranges - change if updating data
stopifnot(cal_end <= 2020)
stopifnot( cal_start >= 1992
           || (i_s == "GLA" && cal_start >= 2000 ) )

# Construct emulated time series
proj_start <- cal_start + nyrs
years_em <- seq( from = proj_start, by = nyrs, to = years_sim[length(years_sim)] )

# End of calibration is in projection period, so check we are predicting this year
stopifnot(cal_end %in% years_em)

cat( paste("Predicting every", nyrs, "years from",
           years_em[1], "to", years_em[length(years_em)], "\n"),
     file = logfile_build, append = TRUE)
cat(paste("with respect to year", cal_start, "\n"), file = logfile_build, append = TRUE)

N_ts <- length(years_em)
cat(paste("Timeslices:", N_ts, "\n"), file = logfile_build, append = TRUE)

#' ## Leave-one-out (LOO) validation choices

do_loo_years <- c(2100, 2150, 2200, 2300)
# (Checks these years are emulated later)

if (do_loo_validation) print(paste("LOO years:", paste(do_loo_years, collapse = ",")))

#' ## Emulation input choices

# Emulator settings ------------------------------------------------------------
#_______________
cat("\nEMULATOR INPUTS:\n", file = logfile_build, append = TRUE)

# // Temps ------------------------------------------------------------

# GSAT timeslices for ice_design
# XXX consider going back earlier?

temps_baseline <- 2015

# Not too many, to avoid linear combinations (esp bad for fixed climate GIS) or overfitting
# Altered later in code if request shorter projections e.g. to 2100 only
if (i_s == "AIS")  temps_list <- 2300
if (i_s == "GIS") temps_list <- 2100 # 2100 is better than 2300 (mostly fixed GSAT after)
if (i_s == "GLA") temps_list <- c(2100, 2300)

# Number of years to average over
# e.g. setting 10 with temps_list = 2300 and temps_baseline = 2015
# gives decadal mean 2291-2300 relative to 2015-2024
N_temp_yrs <- 30

cat(paste("GSAT baseline first year:", temps_baseline, "\n"), file = logfile_build, append = TRUE)
cat(paste("GSAT final year(s):", paste(temps_list, collapse = ","), "\n"), file = logfile_build, append = TRUE)
if (max(temps_list) > max(years_sim)) {
  cat("GSAT timeslice(s) extend beyond ice model simulation: adjusting\n", file = logfile_build, append = TRUE)
  temps_list <- temps_list[ temps_list <= max(years_sim) ]
  if (length(temps_list) == 0) temps_list <- max(years_sim)
  cat(paste("New GSAT input timeslice(s):", paste(temps_list, collapse = ","), "\n"), file = logfile_build, append = TRUE)
}
cat(paste("GSAT period:", N_temp_yrs, "years\n"), file = logfile_build, append = TRUE)

# // Ice model params ----------------------------------------------------------

# Ice model parameters for ice_design


if (i_s == "AIS") {

  # XXX Drop phase, PDD_sd from file; also init method if correlated model
  # (or add PD12 Kori GCM and look at PISM RCM doc)
  # xxx Double-check heat_flux names translation from GCM to RCM ensembles

  ice_cont_list_model <- list()
  ice_factor_list_model <- list()

  # Kori: all
  ice_cont_list_model[["Kori"]] <- c("heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard",
                                     "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")
  ice_factor_list_model[["Kori"]] <- c("melt_param")

  # Kori GCM-forced only
  if ( ensemble_subset %in% c("GCM_forced", "all_forced") ) {
    ice_cont_list_model[["Kori"]] <- c(ice_cont_list_model[["Kori"]],
                                       "lapse_rate", "PDD_ice", "PDD_snow", "refreeze")


    ice_factor_list_model[["Kori"]] <- c( ice_factor_list_model[["Kori"]],
                                          "init_atmos", "init_ocean")

  }

  # Kori RCM-forced only
  if ( ensemble_subset %in% c("RCM_forced", "all_forced") ) {
    ice_cont_list_model[["Kori"]] <- c(ice_cont_list_model[["Kori"]],
                                       "sliding_exponent")
  }

  # PISM
  ice_cont_list_model[["PISM"]] <- c( "heat_flux_PICO" )

  # PISM GCM-forced only
  if ( ensemble_subset %in% c("GCM_forced", "all_forced") ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "sliding_exponent",
                                       "lapse_rate",  "refreeze_frac",
                                       "PDD_ice", "PDD_snow")
    ice_factor_list_model[["PISM"]] <- c( "init_atmos" )
  }

  # PISM RCM-forced only
  if ( ensemble_subset == "RCM_forced" ||
       (ensemble_subset == "all_forced" && final_year <= 2200) ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "overturning_PICO",
                                       "tillwater_decay_rate",
                                       "eff_fraction_overburden_pressure")
  }

  # PISM different resolution between the two
  if ( ensemble_subset == "all_forced" && final_year <= 2200 ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "resolution")
  }


  # CISM
  ice_cont_list_model[["CISM"]] <- c( "resolution",
                                      "heat_flux_ISMIP6_nonlocal",
                                      "heat_flux_ISMIP6_nonlocal_slope")
  # Local only for 2100 runs
  if (final_year == "2100") ice_cont_list_model[["CISM"]] <- c(ice_cont_list_model[["CISM"]],
                                                               "heat_flux_ISMIP6_local")
  ice_factor_list_model[["CISM"]] <- c("melt_param", "sliding_law")

  # Elmer/Ice
  ice_cont_list_model[["ElmerIce"]] <- c("heat_flux_PICO", "sliding_exponent")

  # Combine model lists
  ice_cont_list <- NA
  ice_factor_list <- NA

  for (mm in model_list) {
    if (length(ice_cont_list_model[[mm]]) > 0) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[[mm]])
    if (length(ice_factor_list_model[[mm]]) > 0) ice_factor_list <- c(ice_factor_list, ice_factor_list_model[[mm]])
  }

  # If both models present, can also include this
  # as they use different values
  if ("Kori" %in% model_list && "PISM" %in% model_list) {
    ice_cont_list <- c(ice_cont_list, "overturning_PICO")
  }

  # Drop NA and duplicates
  ice_cont_list <- unique( ice_cont_list[ -1 ] )
  ice_factor_list <- unique( ice_factor_list[ -1 ] )

  # Combine RCM and GCM-forced
  if ( ensemble_subset == "all_forced" && final_year <= 2200 ) {
    ice_factor_list <- c(ice_factor_list, "forcing_type")
  }

  # Add RCM factor (will fail if only using Elmer/Ice)
  # xxx need to add something to
  if ( (ensemble_subset == "all_forced" && final_year <= 2200) ||
       (ensemble_subset == "RCM_forced" && final_year == 2100)) {
    ice_factor_list <- c(ice_factor_list, "RCM")
  }


  # Add model switch and GCM vs RCM-forced factor:
  if ( length(model_list) > 1 ) ice_factor_list <- c(ice_factor_list, "model")

}

# Continuous and categorical (factor) model inputs
if (i_s == "GIS") {

  # xxx Drop SP_climate column - not used and has missing
  # xxx Ignoring retreat_hist for now

  # Individual model lists
  # No factors for GISM
  # xxx Make init_yrs continuous?
  ice_factor_list_model <- list()
  ice_factor_list_model[["CISM"]] <- c("thermodyn", "RCM_init", "init_yrs", "elev_feedback")
  ice_factor_list_model[["IMAUICE"]] <- c("sliding")
  ice_factor_list_model[["ElmerIce"]] <- c("sliding")

  # Combined model lists
  # Continuous parameters
  ice_cont_list <- c("retreat", "resolution")

  # Factors
  # Ignore model_variant as this (sub-name) should be accounted for by other inputs
  ice_factor_list <- "RCM"
  for (mm in model_list) {
    if (length(ice_factor_list_model[[mm]]) > 0) ice_factor_list <- c(ice_factor_list, ice_factor_list_model[[mm]])
  }

  # Drop duplicates
  ice_factor_list <- unique( ice_factor_list )

  # Add model input
  if (length(model_list) > 1) ice_factor_list <- c(ice_factor_list, "model")

}

if (i_s == "GLA") {

  ice_cont_list_model <- list()

  # GloGEM
  ice_cont_list_model[["GloGEM"]] <- c("prec_corr_factor", "ddf_ice",
                                       "ratio_ddf_ice_to_snow",
                                       "prec_gradient" )

  # OGGM
  ice_cont_list_model[["OGGM"]] <- c("prec_corr_factor", "ddf_ice",
                                     "temp_melt", "temp_bias", "glen_a")

  # Both
  ice_cont_list <- NA
  if ("GloGEM" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["GloGEM"]])
  if ("OGGM" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["OGGM"]])
  ice_cont_list <- ice_cont_list[-1]
  ice_cont_list <- unique( ice_cont_list )

  # Ensemble is for any setup differences, e.g.:
  # For OGGM, forcing uses reanalysis 2000-2020 and parameter uses GM
  # For GloGEM, forcing parameters are regional means over glaciers but parameter ensemble has same value everywhere
  if (length(model_list) > 1) { ice_factor_list <- "model"
  } else ice_factor_list <- NA

}

cat(paste("\nContinuous inputs:", paste(ice_cont_list, collapse = " "), "\n"), file = logfile_build, append = TRUE)

# XXX add check that not NA or single value column
# e.g. sliding for CISM is always Schoof

# Combine lists
# assumes always have at least 1 continuous
# but factor might be NA
# Save whether any factors for other uses
if ( ! TRUE %in% is.na(ice_factor_list)) {
  include_factors <- TRUE
  ice_param_list <- c(ice_cont_list, ice_factor_list)
} else {
  include_factors <- FALSE
  ice_param_list <- ice_cont_list
}


if (include_factors) {
  cat(paste("Factors:", paste(ice_factor_list, collapse = " "), "\n"), file = logfile_build, append = TRUE)
}

#' ## Emulator details

# Could set to FALSE if want to check for inert inputs
lower_bound <- TRUE # RobustGaSP default
alpha = NA

if (emulator_settings == "matern_5_2") kernel <- "matern_5_2"
if (emulator_settings == "matern_3_2") kernel <- "matern_3_2"

if (emulator_settings == "pow_exp_01") {
  kernel <- "pow_exp"
  alpha = 0.1
}
if (emulator_settings == "pow_exp_10") {
  kernel <- "pow_exp"
  alpha = 1.0
}
if (emulator_settings == "pow_exp_19") {
  kernel <- "pow_exp"
  alpha = 1.9 # default for pow_exp
}
if (emulator_settings == "pow_exp_20") {
  kernel <- "pow_exp"
  alpha = 2.0
}

stopifnot(kernel %in% c("pow_exp", "matern_5_2", "matern_3_2"))


# Plot: choices ------------------------------------------------------------
#' ## Plot choices

# Plot all or just subset of figures
# 0 for none, 1 for main, 2 for exhaustive
plot_level <- 1
stopifnot(plot_level %in% c(0,1,2)) # using plot_level = 3 to distinguish main.R calls

# Quantiles to output [to text?]
q_list <- c( 0.50, 0.05, 0.95, 0.17, 0.83, 0.25, 0.75 )

# Sub-sample to plot; exclude any dates not predicted by emulator
yy_plot <- c(as.character(cal_end),"2100", "2150", "2200", "2300")
yy_plot <- yy_plot[ yy_plot %in% years_em ]

# Same for LOO timeslices
do_loo_years <- do_loo_years[ do_loo_years %in% years_em ]
if (do_loo_validation && length(do_loo_years) == 0 ) warning("None of requested LOO years are in predictions")

# Match short and full scenario names for plots
scen_name <- list()
for (scen in scenario_list) {
  tmp <- strsplit(scen, split="")[[1]]
  scen_name[[scen]] <- paste( c(tmp[1:4], "-", tmp[5], ".", tmp[6]), collapse = "")
}

# Plot limits for each yy_plot timeslice
sle_lim <- list()
sle_inc <- list()

if (i_s == "AIS") {
  sle_lim[[as.character(cal_end)]] <- c(-4, 8); sle_inc[[as.character(cal_end)]] <- 0.5
  sle_lim[["2050"]] <- c(-10, 90); sle_inc[["2050"]] <- 2
  sle_lim[["2100"]] <- c(-70, 170); sle_inc[["2100"]] <- 5
  sle_lim[["2150"]] <- c(-150, 300); sle_inc[["2150"]] <- 5
  sle_lim[["2200"]] <- c(-250, 500); sle_inc[["2200"]] <- 10
  sle_lim[["2250"]] <- c(-250, 800); sle_inc[["2200"]] <- 10
  sle_lim[["2300"]] <- c(-300, 1000); sle_inc[["2300"]] <- 20
}

if (i_s == "GIS") {  # %in% c("GrIS", "GIS")) {
  sle_lim[[as.character(cal_end)]] <- c(-1, 2); sle_inc[[as.character(cal_end)]] <- 0.1
  sle_lim[["2050"]] <- c(-1, 10); sle_inc[["2050"]] <- 0.5
  sle_lim[["2100"]] <- c(-20, 40); sle_inc[["2100"]] <- 1
  sle_lim[["2150"]] <- c(-50, 100); sle_inc[["2150"]] <- 2
  sle_lim[["2200"]] <- c(-100, 220); sle_inc[["2200"]] <- 5
  sle_lim[["2250"]] <- c(-150, 350); sle_inc[["2200"]] <- 5
  sle_lim[["2300"]] <- c(-200, 450); sle_inc[["2300"]] <- 10
}


if (i_s == "GLA") {

  # Large regions (>1cm)
  # Checked with region 17; special limits for other large regions below
  if (glacier_cap >= 1.0) {
    sle_lim[[as.character(cal_end)]] <- c(-1, 2); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-1, glacier_cap); sle_inc[["2050"]] <- 0.1
    sle_lim[["2100"]] <- c(-2, 1.5*glacier_cap); sle_inc[["2100"]] <- 0.1
    sle_lim[["2150"]] <- c(-3, 2*glacier_cap); sle_inc[["2150"]] <- 0.1
    sle_lim[["2200"]] <- c(-5, 2*glacier_cap); sle_inc[["2200"]] <- 0.1
    sle_lim[["2300"]] <- c(-5, 2*glacier_cap); sle_inc[["2300"]] <- 0.1
  }

  # Adjust lower end for dinky glacier regions (< 1cm)
  if (glacier_cap < 1.0) {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-0.005, glacier_cap); sle_inc[["2050"]] <- 0.1
    sle_lim[["2100"]] <- c(-0.005, 1.1*glacier_cap); sle_inc[["2100"]] <- 0.1
    sle_lim[["2150"]] <- c(-0.005, 1.3*glacier_cap); sle_inc[["2150"]] <- 0.1
    sle_lim[["2200"]] <- c(-0.01, 1.4*glacier_cap); sle_inc[["2200"]] <- 0.1
    sle_lim[["2300"]] <- c(-0.01, 1.5*glacier_cap); sle_inc[["2300"]] <- 0.1
  }

  # Specific region over-rides
  if (reg == "RGI01") {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-10, 1.5*glacier_cap); sle_inc[["2050"]] <- 0.5
    sle_lim[["2100"]] <- c(-10, 1.5*glacier_cap); sle_inc[["2100"]] <- 0.5
    sle_lim[["2150"]] <- c(-15, 2*glacier_cap); sle_inc[["2150"]] <- 0.5
    sle_lim[["2200"]] <- c(-25, 2*glacier_cap); sle_inc[["2200"]] <- 1
    sle_lim[["2300"]] <- c(-70, 2*glacier_cap); sle_inc[["2300"]] <- 1
  }
  if (reg == "RGI19") {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-10, glacier_cap); sle_inc[["2050"]] <- 0.5
    sle_lim[["2100"]] <- c(-20, 1.1*glacier_cap); sle_inc[["2100"]] <- 0.5
    sle_lim[["2150"]] <- c(-25, 1.3*glacier_cap); sle_inc[["2150"]] <- 0.5
    sle_lim[["2200"]] <- c(-35, 1.4*glacier_cap); sle_inc[["2200"]] <- 1
    sle_lim[["2300"]] <- c(-50, 1.5*glacier_cap); sle_inc[["2300"]] <- 1
  }
}

# IPCC AR6 colours
# e.g. see
# https://www.ipcc.ch/site/assets/uploads/2022/09/IPCC_AR6_WGI_VisualStyleGuide_2022.pdf
# https://github.com/IPCC-WG1/colormaps/blob/master/categorical_colors.xlsx

AR6_rgb <- list()
AR6_rgb[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255)
AR6_rgb[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255)
AR6_rgb[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255)
AR6_rgb[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255)
AR6_rgb[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255)

# 60% transparency
AR6_rgb_med <- list()
alpha_med <- 153
AR6_rgb_med[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_med)

# 10% transparency; 20% = 51
AR6_rgb_light <- list()
alpha_light <- 51
AR6_rgb_light[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_light)

# Overshoot colour
# According to
# https://pyam-iamc.readthedocs.io/en/stable/tutorials/ipcc_colors.html
AR6_rgb[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255)
AR6_rgb_med[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_light[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_light)

# PLOT RANGES

# SL simulations are up with glacier cap

# Observations
#if (dataset == "IPCC_AR6") ylim_obs <- c(-1,1.5)
#if (dataset == "PROTECT") {
if (i_s == "GIS") ylim_obs <- c(-1.5,2)
if (i_s == "AIS") ylim_obs <- c(-10,12)

# Hugonnet for glaciers, not IMBIE!
# and do list or similar for regions
if (i_s == "GLA") {
  ylim_obs <- c(-1.5,2)
  if (reg %in% c("RGI12", "RGI18")) ylim_obs <- c(-0.01,0.03)
}
#}


# ________________----
# START ------------------------------------------------------------
#' ## START


#' # Load and process data

#' ## Load observations
# Get obs -------------------------------------------------------------------

# Needs to be before select_sims for history matching filtering of glaciers
obs_data <- emulandice2::load_obs()

#' ## Load climate and ice simulations
# Get sims ---------------------------------------------------------------------

# GET CLIMATE AND LAND ICE SIMULATIONS
# Returns CSV file data
climate_data <- emulandice2::load_sims(variable = "climate") # dataset
ice_data <- emulandice2::load_sims(variable = "ice", source = i_s, region = reg) # dataset

# Index of first column with name format yXXXX
ice_file_yr_start_col <- suppressWarnings( myind <- min(which( nchar(names(ice_data)) == 5
                                                               & substr(names(ice_data), start = 1, stop = 1) == "y"
                                                               & !is.na(as.numeric(substr(names(ice_data), start = 2, stop = 5)) ) ) ) )
# Get full data year range: i.e. from this col to last
ice_file_yr_start <- as.numeric(substr(names(ice_data)[ice_file_yr_start_col], 2, 5))
ice_file_yr_end <- as.numeric(substr(names(ice_data)[length(names(ice_data))], 2, 5))

# Check requested years_sim are within file year range
stopifnot(min(years_sim) >= ice_file_yr_start && max(years_sim) <= ice_file_yr_end )

# Get column number of first ice model input
# which is first one after this list
# Note GCM is included in this list for plots but isn't considered an ice model input elsewhere
ice_param_col_1 <- max(which(c("ice_source", "region", "scenario") %in% names(ice_data))) + 1
# removed "group", "model" so can use as factors

# Get column number of last ice model input
# which is just before first column in y**** format (**** is numeric)
ice_param_col_2 <- ice_file_yr_start_col - 1

# Check requested ice model inputs are in this
ice_param_list_full <- names(ice_data[ice_param_col_1:ice_param_col_2])

cat("\nDeduced ice model inputs from CSV header:\n", file = logfile_build, append = TRUE)
cat(paste(paste(ice_param_list_full, collapse = " "), "\n"), file = logfile_build, append = TRUE)
stopifnot(ice_param_list %in% ice_param_list_full)

# Select sims ---------------------------------------------------------------------

# Select ice source, region, model(s) and any other exclusions
ice_data <- emulandice2::select_sims("main")

# Calculate SLE change w.r.t. cal_start year, and tidy units
ice_data <- emulandice2::calculate_sle_anom()

# Do second selection for glaciers using values of SLE change
if (i_s == "GLA") {
  ice_data <- emulandice2::select_sims("history_match")
}

# Get corresponding forcings (match by GCM + scenario; check length)
temps <- emulandice2::match_sims()

# Find rows that do not have (first timeslice if multiple) forcing
if ( length(temps_list) == 1 ) { sim_index <- !is.na(temps)
} else sim_index <- !is.na(temps[,1])

# Exclude these from both ice and climate data
ice_data <- ice_data[ sim_index, ]
if ( length(temps_list) == 1) { temps <- temps[ sim_index ]
} else temps <- temps[ sim_index, ]

# END OF ICE SIMULATION SELECTION
N_sims <- dim(ice_data)[1]

cat(paste("\nFINAL DATA SELECTION: using", N_sims, "ice simulations for",
          i_s, reg, "\n"), file = logfile_build, append = TRUE)

# Check some simulations found!
stopifnot(N_sims > 0)

print("After selection:")

# Ice sheet regions ------------------------------------------------------------

if (i_s %in% c("AIS","GIS")) {

  cat("\nIce sheet regional fractions\n", file = logfile_build, append = TRUE)

  # Get row numbers i.e. selected simulations of main dataset
  sims_index <- rownames(ice_data[ ,  paste0("y", years_em) ])

  # No need to run calculation_sle_anom: just anomaly and x100

  region_names <- list() # names of regions
  region_fracs_all <- list() # histograms for each region
  region_fracs <- list() # mean or adjusted median fraction for each region

  # Calculate mean fractions for regions
  if (i_s == "GIS") {

    # This file has ALL + 6 regions
    # xxx issue: Remake after deliverable: this is sle not slc
    region_file <- read.csv(paste0( inputs_preprocess, "/GIS/SLE_SIMULATIONS_GIS_p9_240304.csv"))

    # Translate CSV regions to nicer names for netcdf files
    region_names[["nw"]] <- "NW"
    region_names[["no"]] <- "NO"
    region_names[["cw"]] <- "CW"
    region_names[["ne"]] <- "NE"
    region_names[["sw"]] <- "SW"
    region_names[["se"]] <- "SE"

    # All simulations (to construct index)
    # xxx could use load_sims here
    all <- region_file[ region_file$region == "ALL",  ]
    nrows_all <- dim(all)[1]

    # Timeslices for sims selected in main analysis
    all <- all[ sims_index, paste0("y", years_em) ]

    # Plot: GIS regions ----------------------------------------------------------

    # Open plot file for histograms
    # xxx use prefix name as for other pdfs
    if (plot_level > 0) {
      pdf( file = paste0( outdir, "region_fractions_", i_s, ".pdf" ))
      par(mfrow = c(3,2))
    }

    for (rr in names(region_names) ) {

      # Get all simulations for region and number rows
      region_all <- region_file[ region_file$region == rr, paste0("y", years_em) ]
      rownames(region_all) <- 1:nrows_all

      rr_name <- region_names[[rr]]

      # Pick same rows as main analysis
      region_all <- region_all[ sims_index, ]

      # Calculate fractions (all timeslices in all simulations)
      region_fracs_all <- unlist(  region_all / all )

      # Mean of these
      region_fracs[[ rr_name ]] <- mean(region_fracs_all, na.rm = TRUE)

      # Print to file
      cat( sprintf( "%s: %.4f\n", rr_name,
                    region_fracs[[ rr_name ]] ), file = logfile_build, append = TRUE)

      # Plot
      if (plot_level > 0) {

        hist(region_fracs_all, xlim = c(0,1),
             breaks = seq(from = floor(min(region_fracs_all, na.rm = TRUE)),
                          to = ceiling(max(region_fracs_all, na.rm = TRUE)), by = 0.01),
             main = paste0(ice_name, ": ", rr_name), xlab = "Fraction" )
        abline(v = region_fracs[[ rr_name ]], lwd = 2, col = "blue")
        text( 0.7, 300, sprintf("Mean: %.3f",
                                region_fracs[[ rr_name ]]), col = "blue")
      }


    }

    if (plot_level > 0) dev.off()

    cat( paste("\nTotal:", sum(unlist(region_fracs)), "\n"), file = logfile_build, append = TRUE)

  }

  # Calculate adjusted mean fractions for regions
  if (i_s == "AIS") {

    region_names <- c( "WAIS1", "WAIS2", "WAIS3", # ASE, Ross, RF
                       "PEN",
                       paste0("EAIS", 1:7) )

    # All simulations (to construct index)
    all <- emulandice2::load_sims(variable = "ice", source = i_s)

    # Drop Phase 1 before getting num. simulations because none in regional files
    all <- all[ all$Phase != 1 | is.na(all$Phase), ]
    nrows_all <- dim(all)[1]

    # Timeslices for sims selected in main analysis
    all <- all[ sims_index, paste0("y", years_em) ]

    for (rr in 1:length(region_names) ) {

      # Regional CSV
      # xxx Could add region arg to load_sims?
      region_file <- read.csv(paste0( inputs_preprocess, "/AIS/regions/SLE_SIMULATIONS_AIS_full_region_",rr,"_240527.csv"))

      rr_name <- region_names[[rr]]

      # Get all simulations for region and number rows
      region_all <- region_file[ , paste0("y", years_em) ]
      rownames(region_all) <- 1:nrows_all

      # Pick same rows as main analysis
      region_all <- region_all[ sims_index, ]

      # Calculate fractions (all timeslices in all simulations)
      region_fracs_all[[ rr_name ]] <- as.numeric(unlist(region_all))  / as.numeric(unlist(all))

      # Replace infinities with missing
      region_fracs_all[[ rr_name ]][is.infinite(region_fracs_all[[ rr_name ]])] <- NA

      # Calculate MEDIAN not mean for Antarctica
      region_fracs[[ rr_name ]] <- median(region_fracs_all[[ rr_name ]], na.rm = TRUE)

      # Print to file
      cat( sprintf( "%s: %.4f\n", rr_name,
                    region_fracs[[ rr_name ]] ), file = logfile_build, append = TRUE)

    }

    total_median <- sum(unlist(region_fracs))

    cat( paste("\nTotal of medians:", total_median, "\n"), file = logfile_build, append = TRUE)

    missing <- 1.0 - total_median
    cat(sprintf("\nMissing fraction before adjustment: %.3f\n", missing), file = logfile_build, append = TRUE)

    # Get median fractions to adjust
    region_fracs_adj <- unlist(region_fracs)

    # Sort sectors - this is from when I redistributed only to largest sectors
    cat("\nSectors in decreasing contribution:\n", file = logfile_build, append = TRUE)
    sec_sort <- sort(unlist(region_fracs_adj), decreasing = T, index.return = T)
    for (ss in sec_sort$ix) {
      cat(sprintf("%i: %.1f%%\n", ss, 100.0*region_fracs_adj[ss]), file = logfile_build, append = TRUE)
    }

    # Now redistribute amongst all regions instead
    n_largest <- length(region_names)

    cat(paste("\nTake largest",n_largest,"sub-sectors:\n"), file = logfile_build, append = TRUE)
    largest <- sec_sort$ix[1:n_largest]
    cat(largest, "\n", file = logfile_build, append = TRUE)

    cat("\nNormalise these, from:\n", file = logfile_build, append = TRUE)
    cat(region_fracs_adj[largest],"\n", file = logfile_build, append = TRUE)

    # Proportion of total of this subset
    prop <- region_fracs_adj[largest] / (sum(region_fracs_adj[largest]))
    cat("to:\n", file = logfile_build, append = TRUE)
    cat(prop, "\n", file = logfile_build, append = TRUE)
    stopifnot(sum(prop) - 1 < 0.0001)

    tot_adj <- 0.0
    tot_adj_largest <- 0.0

    # Plot: AIS regions ----------------------------------------------------------

    # Pdf later than for GIS because adjusting fractions
    if (plot_level > 0) {
      # xxx Use prefix name - can probably move both outside i_s chunks
      pdf( file = paste0( outdir, "region_fractions_", i_s, ".pdf" ))
      par(mfrow = c(3,2))
    }

    cat("\nAdjust median fractions to sum to 1:\n", file = logfile_build, append = TRUE)

    for (ss in 1:length(region_names)) {

      # Using approx. fraction
      if (ss %in% largest) {

        miss_bits <- prop[ which(ss == largest, arr.ind = T) ] * missing
        region_fracs_adj[ss] = region_fracs_adj[ss] + miss_bits


        tot_adj_largest <- tot_adj_largest + region_fracs_adj[ss]

        cat( sprintf("%i: median = %.3f, adjusted = %.3f (%.0f%% adjustment)\n",
                     ss, region_fracs[[ss]], region_fracs_adj[ss], 100.0*miss_bits / region_fracs[[ss]] ),
             file = logfile_build, append = TRUE)

      } else {

        cat( sprintf("%i: median = %.3f\n",
                     ss, region_fracs_adj[ss]), file = logfile_build, append = TRUE )

      }

      tot_adj <- tot_adj + region_fracs_adj[ss]


      # Plot histograms now so can show median and adjusted together
      if (plot_level > 0) {
        hist(region_fracs_all[[ ss ]], xlim = c(-1,1),
             breaks = seq(from = floor(min(region_fracs_all[[ ss ]], na.rm = TRUE)),
                          to = ceiling(max(region_fracs_all[[ ss ]], na.rm = TRUE)), by = 0.01),
             main = paste0(ice_name, ": ", region_names[[ss]]), xlab = "Fraction" )
        abline(v = region_fracs[[ ss ]], lwd = 2, col = "darkred")
        abline(v = region_fracs_adj[ ss ], lwd = 2, col = "red", lty = 2)

        # xxx sort ypos for 2300 and 2100
        text( 0.45, 800, pos = 4, sprintf("Median: %.3f",
                                          region_fracs[[ ss ]]), col = "darkred", cex = 0.9)
        text( 0.45, 400, pos = 4, sprintf("Adjusted: %.3f",
                                          region_fracs_adj[ ss ]), col = "red", cex = 0.9)
      }

      # Overwrite original fraction list with adjusted AFTER plotting histograms
      region_fracs[[ss]] <- region_fracs_adj[ss]


    }

    if (plot_level > 0) dev.off()

    cat(sprintf("\nTotal of largest %i sectors after adjustment: %.3f\n", n_largest, tot_adj_largest), file = logfile_build, append = TRUE )
    cat(sprintf("Total of all sectors after adjustment = %.3f\n", tot_adj), file = logfile_build, append = TRUE)

  }

} # ice sheet regions

# Final checks ------

# Degrees of freedom check: do we have enough simulations (rows)
# for predicting timeslices (columns)?
stopifnot(N_sims > N_ts)

# Check multiple values for the inputs, otherwise fail
for ( pp in ice_param_list ) {
  if (length( unique(ice_data[,pp]) ) == 1) {
    stop( paste("Only one unique value of requested input", pp, "in dataset: please drop"))
  }
}

# Check for NAs in columns we plan to use to emulate, otherwise fail
cols_to_check <- ice_data[ , ice_param_list ]
if (length(cols_to_check[ is.na(cols_to_check) ]) > 0) stop("NAs found in ice_data columns to use as inputs in emulation: please drop/fix")

# Select continuous model inputs and impute missing values
ice_param <- emulandice2::get_inputs(ice_cont_list)

# COMBINE CLIMATE FORCING AND ICE MODEL INPUTS INTO DESIGN MATRIX
ice_design <- as.matrix( data.frame(temps, ice_param) )


# Add climate col name(s0)
# xxx Can use this elsewhere! e.g. plot_design.R instead of reconstructing
temps_list_names <- paste0("GSAT_", temps_list)
colnames(ice_design)[ 1:length(temps_list) ] <- temps_list_names

# Create axis label for plots
GSAT_lab <- list()
for (tt in 1:length(temps_list_names)) {
  GSAT_lab[[temps_list_names[tt]]] <- paste0('Global mean temperature ',
                                             temps_list[tt]-N_temp_yrs+1,'-',temps_list[tt],
                                             ' rel. to ',temps_baseline,'-',temps_baseline+N_temp_yrs-1,' (degC)')
}

# One-hot encoding ---------------------------------------------------------------
#' ## One-hot encoding of factors

ice_factor_values <- list()

# ADD FACTOR COLUMNS
if ( ! TRUE %in% is.na(ice_factor_list)) {

  # Adding factors
  for ( ff in ice_factor_list ) {

    cat(paste("Factor to add:", ff, "\n"), file = logfile_build, append = TRUE)
    ff_vals <- sort(unique(ice_data[ ,ff]))

    cat(paste("Levels:", length(ff_vals), "\n"), file = logfile_build, append = TRUE)

    # Take first alphabetical value as reference/nominal
    ff_ref <- ff_vals[1]

    cat(paste("Adding dummy variables with reference value:", ff_ref, "\n"), file = logfile_build, append = TRUE)

    for ( vv in ff_vals ) {

      # Drop one level to avoid collinearity
      if (vv == ff_ref) next

      # Name of column is factor:level
      cat(paste0("Generating column ", ff, ":", vv, "\n"), file = logfile_build, append = TRUE)
      dummy <- ifelse(ice_data[, ff] == vv, 1, 0 )
      ice_design <- cbind(ice_design, dummy)
      colnames(ice_design)[dim(ice_design)[2]] <- paste(ff, vv, sep = ":")
    }
    # Alternative code
    # for (j in 1:length(ff_vals)) dummy[,j] <- as.integer(ice_data[, ff] == ff_vals[j])

    #}
    # Save to use for prior
    ice_factor_values[[ff]] <- ff_vals
  } # factors loop
}

# Save list of continuous inputs in design
input_cont_list <- c(temps_list_names, ice_cont_list)

# Save list of ice inputs: not ice_param_list but expanded dummy versions
ice_all_list <- ice_cont_list
ice_dummy_list <- NA
if (include_factors) {
  ice_dummy_list <- colnames(ice_design)[ ! colnames(ice_design) %in% input_cont_list]
  ice_all_list <- c( ice_all_list, ice_dummy_list)
}


#' ## Scale inputs for emulator
# Scale inputs ---------------------------------------------------------------
# xxx Can I move this into get_inputs()?

cat("\nOriginal ranges of inputs:\n", file = logfile_build, append = TRUE)
for (cc in 1:dim(ice_design)[2]) {
  cat( paste(colnames(ice_design)[cc], min(ice_design[,cc]), "to",
             max(ice_design[,cc]), "\n"), file = logfile_build, append = TRUE)
}

# Scale columns of continuous parameters (climate and ice model)
cat("\nCentre and scale continuous inputs (mean = 0, s.d. = 1)\n", file = logfile_build, append = TRUE)
ice_design_scaled_cont <- scale(ice_design[, input_cont_list])

# Store scaling to use later for prior
inputs_centre <- attr(ice_design_scaled_cont,"scaled:center")
inputs_scale <- attr(ice_design_scaled_cont,"scaled:scale")

# Fill back into original design
ice_design_scaled <- ice_design
ice_design_scaled[, input_cont_list] <- ice_design_scaled_cont

cat("\nNew ranges of inputs after scaling:\n", file = logfile_build, append = TRUE)
for (cc in 1:dim(ice_design_scaled)[2]) {
  cat( paste(colnames(ice_design_scaled)[cc], min(ice_design_scaled[,cc]), "to",
             max(ice_design_scaled[,cc]), "\n"), file = logfile_build, append = TRUE)
}

# Make sure scenario list only includes those of simulations
scenario_list <- scenario_list[ scenario_list %in% unique(ice_data[,"scenario"]) ]
#cat(paste("Scenario list:",paste(scenario_list, collapse = ","), "\n"), logfile_build, append = TRUE)


#' # Plot simulations
# Plot: sims -----------------------------------------------------------------------

cat("\nPlot simulator projections:\n", file = logfile_build, append = TRUE)

# Plot simulations (some with observations)
# Can repeat from main.R to tweak or add model discrepancy to history matching window
if (plot_level > 0) {
  pdf( file = paste0( outdir, out_name, "_SIMS.pdf"),
       width = 9, height = 5)
  emulandice2::plot_designs("sims", plot_level)
  emulandice2::plot_timeseries("sims", plot_level)
  emulandice2::plot_scatter("sims", "none", plot_level)
  emulandice2::plot_distributions("sims", plot_level)
  dev.off()
}


#_______________________________________________________________________________

# Settings are hard-coded at the top
# Writes emu obj from .RData workspace file later for running in FACTS

# Could use this for now if exists and unchanged: if ( ! exists("emu_mv") )

# FUNCTION NOT WORKING SINCE A PACKAGE - SEE CODE ABOVE INSTEAD
# if (FALSE) {

# Build emulator for timeslices every nyrs
# Note this call is repeated in do_LOO.R
if ( ! exists("emu_mv") ) {
  #show(system.time(
  emu_mv <- emulandice2::make_emu( ice_design_scaled,
                      as.matrix( ice_data[ , paste0("y", years_em) ] ))
  #))
} else {
  message("Not rebuilding emulator! Use only for testing code or slow build emulators")
}

#} # don't build emu_mv from function make_emu()


# ________________----
# TEST ------------------------------------------------------------

#' # Predict for SA designs
# Design: main effects -----------------------------------------------------------------------

#' ## Main effects
# Main effects (i.e. one-at-a-time design for sensitivity analysis)
design_sa <- emulandice2::load_design_to_pred("main_effects")

# Predict: overwrite object
myem <- list()
for (input in names( design_sa )) {

  cat(paste("Main effects:",input,"\n"), file = logfile_build, append = TRUE)

  design_sa_scaled_cont <- scale(design_sa[[input]][ , input_cont_list],
                                 center = inputs_centre,
                                 scale = inputs_scale )

  design_sa_scaled <- as.data.frame( design_sa[[input]] )
  design_sa_scaled[ , input_cont_list ] <- design_sa_scaled_cont
  myem[[input]] <- emulandice2::emulator_predict( design_sa_scaled )
}

#' ## Uniform temperature prior

# Design: uniform ----------------------------------------------------------------------

# Design "unif_temps" makes projections using uniform priors for GSAT with same ranges as sims
# a better comparison than using FaIR projected distributions for each SSP

design_pred <- emulandice2::load_design_to_pred("unif_temps")

for (scen in scenario_list) {

  cat(paste("Scenario with uniform priors:",scen,"\n"), file = logfile_build, append = TRUE)

  design_pred_scaled_cont <- scale(design_pred[[scen]][ , input_cont_list],
                                   center = inputs_centre,
                                   scale = inputs_scale )
  design_pred_scaled <- as.data.frame( design_pred[[scen]]  )
  design_pred_scaled[ , input_cont_list] <- design_pred_scaled_cont
  myem[[scen]] <- emulandice2::emulator_predict( design_pred_scaled )
}

# Sample emu uncertainty ----------------------------------------------------------------------
projections <- list()

# Want to see unif_temps final projections (samples with uncertainty) for validation
for (scen in scenario_list) {
  projections[[scen]] <- emulandice2::emulator_uncertainty(myem[[scen]])
}

# Plot: SA -----------------------------------------------------

# Plot sensitivity analysis
if (plot_level > 0) {
  pdf( file = paste0( outdir, out_name, "_SA.pdf"),
       width = 9, height = 5)
  emulandice2::plot_scatter("prior", "main_effects", plot_level)
  emulandice2::plot_scatter("prior", "unif_temps", plot_level)
  emulandice2::plot_scatter("posterior", "unif_temps", plot_level) # overkill?
  dev.off()
}


#' # Validate

# Validate ---------------------------------------------------------------------
# Builds LOO multivariate emulators and keeps results for requested timeslices
if (do_loo_validation) {

  cat("\nLEAVE ONE OUT VALIDATION\n", file = logfile_build, append = TRUE)

  # Test every N_k-th run
  # this is the slow bit....
  # xxx Improve: stratified by output value instead of every N_k
  loo_valid_all <- emulandice2::do_loo( do_loo_years, N_k = N_k)

  # To store results
  loo_mean <- list()
  loo_sd <- list()
  wrong <- list()

  # Loop over time slices to calculate metrics and make plots
  for ( yy in do_loo_years) {

    yind <- paste0( "y", yy)

    # Get LOO prediction (in do_loo.R)
    loo_mean[[yind]] <- loo_valid_all$mean[ , yind]
    loo_sd[[yind]] <- loo_valid_all$sd[ , yind]

    # N_k selection of runs
    N_k_index <- !is.na(loo_mean[[yind]])
    N_k_subset <- length( loo_mean[[yind]][ N_k_index ]  )

    # Which ones were within predicted intervals and which ones missed?
    wrong[[ yind ]] <- ice_data[ , yind] > ( loo_mean[[yind]] + 2*loo_sd[[yind]] ) |
      ice_data[ , yind] < (loo_mean[[yind]]  - 2*loo_sd[[yind]])

    # Fraction that missed
    frac_right <- 1 - ( length(which(wrong[[yind]][N_k_index] == TRUE)) / N_k_subset )

    # xxx Could save in list for plot_loo, or output summary there - duplication
    loo_err <- loo_mean[[yind]] - ice_data[ , yind ]
    loo_std_err <- loo_err / loo_sd[[yind]]

    # PRINT RESULTS
    cat(paste("\nLOO VALIDATION:",yy, "\n"), file = logfile_build, append = TRUE)
    cat(sprintf("Number within emulator 95%% intervals: %.2f%%\n",
                frac_right*100.0), file = logfile_build, append = TRUE)
    cat(sprintf("Mean of absolute errors (cm): %.1f\n",
                mean(abs(loo_err[ N_k_index ]))), file = logfile_build, append = TRUE)
    cat(sprintf("Range of absolute errors (cm): [%.1f, %.1f]\n",
                min(loo_err[ !is.na(loo_err)]), max(loo_err[ N_k_index ])),
        file = logfile_build, append = TRUE)
    cat(sprintf("Mean of standardised errors: %.1f\n",
                mean(loo_std_err[ N_k_index ])), file = logfile_build, append = TRUE)
    cat(sprintf("Range of standardised errors: [%.1f, %.1f]\n",
                min(loo_std_err[ N_k_index ]), max(loo_std_err[ N_k_index ])),
        file = logfile_build, append = TRUE)

  } # years

  # Plot: LOO-------
  # Plot LOO results
  pdf( file = paste0( outdir, out_name, "_LOO.pdf"),
       width = 9, height = 5)
  emulandice2::plot_loo()
  dev.off()

} # do_loo_validation

#' # Save emulator build file

# SAVE EMULATOR BUILT FROM WHOLE ENSEMBLE
# and the rest of the workspace, at least for now
RData_file <- paste0(rdatadir, out_name, "_EMULATOR.RData")
save.image( file = RData_file )

cat(paste("\nSaved RData file:",RData_file,"\n"), file = logfile_build, append = TRUE)






