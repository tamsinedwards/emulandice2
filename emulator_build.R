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
# in data-raw/ to be read by FACTS for predicting land ice contributions
# Other output is written to out/
#
# Set plot_level > 0 to output plot pdf files
#_______________________________________________________________________________

# SETUP ------------------------------------------------------------------------
#' # SETUP

# Get arguments from RScript
args <- commandArgs(TRUE)

# Optional argument: config filename
config_filename <- NA

# Defaults if no args set (used for testing and Markdown)
if (length(args) == 0) {

  warning("No arguments set - using defaults")
  i_s <- "AIS"
  reg_arg <- "ALL"
  final_year <- 2150

} else {

  # Ice source and final year ----------------------------------------------------------
  #' # Choose ice source and final year

  # At least 3 args; for now constrain to 4
  stopifnot(length(args) >= 3 && length(args <= 4))

  # Ice source
  i_s <- args[1]

  # Region (name for AIS; number for GLA)
  reg_arg <- args[2]

  # End year
  final_year <- as.numeric(args[3]) # if past 2100, applies model/ensemble selections later

  # Configuration file (optional)
  if (length(args) >= 4) config_filename <- args[4]

}

# Check ice source name
stopifnot(i_s %in% c("GIS","AIS", "GLA"))

# Region is checked/set here

# ICE SHEET / SECTOR
if (i_s == "GIS") stopifnot( reg_arg == "ALL" )
if (i_s == "AIS") stopifnot( reg_arg %in% c("WAIS", "EAIS", "PEN", "ALL") )
reg <- reg_arg

# RGI NUMBER
if (i_s == "GLA") {
  reg_num <- as.numeric(reg_arg)
  stopifnot( reg_num %in% 1:19 )
  reg <- paste0("RGI", sprintf("%02i", reg_num)) # zero-padded
}

# Double-check region name is valid
stopifnot(reg %in% c("ALL", "WAIS", "EAIS", "PEN", paste0("RGI", sprintf("%02i",1:19))))

# Fix random seed
set.seed(2024)

# Directory for output files
out_stub <- paste0(i_s,"_",reg,"_", final_year)
rdatadir <- "./data-raw/" # RData file containing emulator
outdir <- "./out/" # Everything else
plotdir <- paste0("./out/",out_stub,"_build_plots/") # Plot subdir

# Create directories if they do not exist (may not if running on HPC)
# Plot dir is created later - set by plot_level
if ( ! file.exists(rdatadir) ) dir.create(file.path(rdatadir))
if ( ! file.exists(outdir) ) dir.create(file.path(outdir))

# Directories for input datasets
# (all in the same place, but grouped by type in case want to change structure)
inputs_preprocess <- paste0(system.file("extdata", package = "emulandice2"), "/")
inputs_ext <- inputs_preprocess

# Get configuration file for ice source region
# Default if not set as argument
if (is.na(config_filename)) config_filename <- paste0("config_",i_s,"_",reg,".yml")
print(paste0("Looking for configuration file: ./inst/", config_filename))
config_file <- system.file(config_filename,
                           package = 'emulandice2', mustWork = TRUE)
print(paste("Configuration file:", config_file))

# Analysis choices ------------------------------------------------------------------------

#' # Analysis choices
#' ## Dataset, ice source, region [ensemble]

# Switch to go back to PROTECT deliverable settings (not tested!)
dt <- config::get("deliverable_test", file = config_file)
deliverable_test <- if (is.null(dt)) FALSE else isTRUE(dt)

# Just read, filter and plot simulations (for testing etc)
# Default is off
sims_only <- config::get("sims_only", file = config_file)
read_sims_only <- ifelse( !is.null(sims_only), sims_only, FALSE)

# Calculate ice sheet regional fractions from dataset
# Flag is also used by write_outputs.R during prediction
# i.e. do fractions exist so should fractional files be written
do_regions <- TRUE

# Impute missing years in simulations: either a light fill, or an extension
# Option "none" will currently fail in make_emu SVD if missing value(s): xxx add something to skip runs?
# Extend will only use the simulation if forcing exists (often doesn't)

# Defaults: type and number of missing years allowed
impute_sims <- "fill"
impute_nyrs <- 5  # restricted fill

if (i_s == "AIS" && final_year == "2150") {
  impute_sims <- "extend"
  impute_nyrs <- 50
}
if (i_s == "GIS" && final_year == "2300") {
  impute_sims <- "extend"
  impute_nyrs <- 100
}
if (deliverable_test) impute_sims <- "none"
stopifnot(impute_sims %in% c("none", "fill", "extend"))

if (impute_sims == "none") impute_nyrs <- 0

# Later there are options to pick sub-ensembles (obsolete / not used?)
ensemble_subset <- NA


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
  stopifnot(final_year %in% c(2100, 2150, 2300))
}

dh <- config::get("do_history_match", file = config_file)
do_history_match <- if (is.null(dh)) TRUE else isTRUE(dh)

# Model discrepancy scaling for history-match pre-screen (used in select_sims)
if (do_history_match) {
  sm <- config::get("scale_mod_err_sel", file = config_file)
  scale_mod_err_sel <- if (is.null(sm)) 5.0 else as.numeric(sm)
  stopifnot(is.finite(scale_mod_err_sel), scale_mod_err_sel >= 0)
}

# SVD scree threshold for make_emu / do_loo (YAML overrides GIS bump)
st <- config::get("scree_thresh", file = config_file)
if (is.null(st)) {
  scree_thresh <- 0.999
  if (i_s == "GIS" && final_year >= 2200) scree_thresh <- 0.99999
} else {
  scree_thresh <- as.numeric(st)
}
stopifnot(is.finite(scree_thresh), scree_thresh > 0, scree_thresh <= 1)

# Plot all or just subset of figures: 0 none, 1 main, 2 exhaustive
pl <- config::get("plot_level", file = config_file)
plot_level <- if (is.null(pl)) 2L else as.integer(pl)
stopifnot(plot_level %in% c(0L, 1L, 2L)) # plot_level = 3 used to distinguish main.R calls
if ( plot_level > 0 && ! file.exists(plotdir) ) dir.create(file.path(plotdir))

# Set max ensemble size for training GPs in TVT validation - optionally set in config file
# Uses minimum of this or 70% of dataset for train and test validation
# e.g. if 1000, then trains on 700 and uses 300 for testing
# if 1500, then trains on 1000 and uses 500 for testing
# if 2000, then still trains on 1000 and uses 1000 for testing
# Or can set to NA, e.g. for laGP which can handle large data
N_max_em <- config::get("n_train", file = config_file)

# If not set, limit to max GP can handle
if (is.null(N_max_em) || N_max_em > 1000L) N_max_em <- 1000L

# Long names for outputs
if (i_s == "GIS") ice_name <- "Greenland"
if (i_s == "AIS") ice_name <- "Antarctica"
if (i_s == "GLA") {
  ice_name <- read.csv(paste0(inputs_ext,"/GLA/regionnames.txt"), header = FALSE)[reg_num,1]
}

# Sample size for unif_temps design - used for convenience when adding uncertainty
# (Main effects sample size is set in load_design_to_pred.R, and
# AR6 prior sample is equal to number of GSAT projections)
N_unif <- 2000L

# Validation type (tvt / loo)
vt <- config::get("validation_type", file = config_file)
validation_type <- if (is.null(vt)) "tvt" else vt
stopifnot(validation_type %in% c("tvt", "loo"))

# Subsample for LOO
N_k <- NA # integer for every N_k-th simulation; NA for full LOO # xxx add switch by size?

# May as well switch on full LOO if GIS 2300 (quick)
if (deliverable_test) {
  if (i_s == "GIS" && final_year > 2200) {
    validation_type <- "loo"
    N_k <- NA
  }
}

# Glacier dataset for calibration
if (i_s == "GLA") {
  gd <- config::get("glacier_data", file = config_file)
  glacier_data <- if (is.null(gd)) "GlaMBIE" else gd
  stopifnot(glacier_data %in% c("Hugonnet", "GlaMBIE"))
}

print("************************************************************************************************")
print("Hello! Welcome to emulandice2: build")
print("************************************************************************************************")

print(paste(ice_name,"region",reg))
if (read_sims_only) print("ONLY READING SIMULATIONS")
print(paste0("Config file: ./inst/", config_filename))
if (!read_sims_only) {
  if (validation_type == "loo") {
    print(paste("Using LOO validation with N_k =",ifelse(is.na(N_k), "all", N_k),"(could be very slow)"))
  } else {
    print(paste("Using TVT validation after training on up to",N_max_em,"simulations"))
  }
}

#' ## Projection times and possible scenarios

# SIMULATION YEARS in dataset i.e. columns in CSV

# First year of simulations we want to use
# checks later this is within CSV file header range
if (i_s == "AIS") first_year <- 1950
if (i_s == "GIS") first_year <- 1960
if (i_s == "GLA") first_year <- 1980

years_sim <- first_year:final_year

# Timeslice frequency to predict after break year
# (see below)
nyrs <- 5

if (deliverable_test) nyrs <- 5

# Check reasonable choice
stopifnot(nyrs %in% c(1, 2, 5, 10))

# Full list of possible emissions scenarios to look for
# (dropped from unif_temps design if not simulated)
# over-recon is Heiko's reconstruction of SSP534-over forcing

# Scenario list for plots and projections (not selecting simulations)
# XXX SHOULD I ADD RCPS FOR GIS?? CHECK WHERE USED IN SIM PLOTS
scenario_list <- c("SSP119", "SSP126", "SSP245", "SSP370", "SSP534-over", "SSP534-over-recon", "SSP585")
if (deliverable_test) scenario_list <- c("SSP119", "SSP126", "SSP245", "SSP370", "SSP585")

#' ## Ice model(s)

if (i_s == "AIS") {

  # All models (do not change!)
  model_list_full <- c( "Kori", "PISM", "CISM", "ElmerIce", "BISICLES", "IMAUICE", "UFEMISM" )
  if (deliverable_test) model_list_full <- c( "Kori", "PISM", "CISM", "ElmerIce" )

  # Would drop short simulations anyway but early on is better for emulator inputs
  if (ensemble_subset == "GCM_forced" ||
      (ensemble_subset == "all_forced" && final_year > 2200) ) {
    model_list <- c( "Kori", "PISM", "BISICLES", "IMAUICE" )
    if (deliverable_test) model_list <- c( "Kori", "PISM")
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
  # for CISM runs only
  # Since 250719, have excluded all but a few 2300 for keeping: so set to FALSE
  need_retreat_match <- FALSE
  if (deliverable_test) need_retreat_match <- TRUE

  # Only CISM went beyond 2100 (at all / to any great extent)
  if ( final_year > 2100 &&
       ( length(model_list) > 1 ||
         (length(model_list) == 1 && model_list != "CISM") ) ) return()

}

if (i_s == "GLA") {

  # All models (do not change!)
  model_list_full <- c( "GloGEM", "OGGM", "GO" )

  # Pick models, or set to model_list_full to use all
  model_list <- model_list_full

  # Fraction of glaciers that must have completed (guidance from Fabien Maussion)
  # Selection is done in select_sims()
  # Only OGGM and GO have completion % information, not GloGEM
  # Completion thresholds from Meg James; overridable in YAML
  ct_oggm <- config::get("complete_thresh_oggm", file = config_file)
  ct_go   <- config::get("complete_thresh_go", file = config_file)
  complete_thresh <- list(
    OGGM = if (is.null(ct_oggm)) 0.90 else ct_oggm,
    GO   = if (is.null(ct_go))   0.85 else ct_go
  )
  # No GO at time of PROTECT deliverable
  if (deliverable_test) complete_thresh[["OGGM"]] <- 0.80

}

# Check selected model names are correct
stopifnot( length( setdiff(model_list, model_list_full )) == 0 )

# Emulator choices ------------------------------------------------------------------------

# Stationary (RobustGaSP), local, or deep Gaussian Process emulator (mandatory in YAML)
emulator_type <- config::get("emulator_type", file = config_file)
stopifnot(!is.null(emulator_type),
          emulator_type %in% c("statGP", "laGP", "deepgp", "dgpsi"))

N_mcmc <- NA
if (emulator_type == "deepgp") N_mcmc <- 100L

#' ## Set emulator covariance function
# Choose emulator covariance function here so can put in output name for now

if (emulator_type == "statGP") {

  # Can choose matern_5_2, matern_3_2,
  # or pow_exp (power-exponential with alpha = 0.1, 1.0, 1.9, 2.0)
  # Could add

  # XXX Specify by ice sheet sector later if using
  # Mandatory in YAML for statGP
  emulator_covar <- config::get("emulator_covar", file = config_file)

  stopifnot(!is.null(emulator_covar),
            emulator_covar %in% c("matern_5_2", "matern_3_2",
                                  "pow_exp_01", "pow_exp_10",
                                  "pow_exp_19", "pow_exp_20"))
}

if (emulator_type == "deepgp") {

  # Squared exponential ("gauss" in RobustGaSP) or Matern
  # Matern smoothness is v=2.5 by default, i.e. matern_5_2
  emulator_covar <- "matern" # exp2"
  stopifnot(emulator_covar %in% c("exp2", "matern"))

}

if (emulator_type == "dgpsi") {
  # sexp is default; alternative is matern2.5 — mandatory in YAML for dgpsi
  emulator_covar <- config::get("emulator_covar", file = config_file)
  stopifnot(!is.null(emulator_covar))
}

# Set here because of conditionals in make_emu.R
laGP_scaling <- FALSE

if (emulator_type == "laGP") {

  emulator_covar <- "exp2" # default in laGP
  stopifnot(emulator_covar == "exp2")

  laGP_method <- "alcray" # alc, alcray (faster but worse)
  laGP_nugget_prior <- 0.1 # prior value for nuggets

}

#' ## Open output file

# Date stamped name stem for output txt and log files
out_name <- paste0(out_stub, "_", format(Sys.time(), "%y%m%d") )
logfile_build <- paste0(outdir, out_name,"_build.txt")

#______________________________________________________
# START WRITING LOG FILE
cat("_____________________________________\n", file = logfile_build)

cat(paste("LAND ICE SOURCE:", ice_name, reg, "\n"), file = logfile_build, append = TRUE)

cat(paste0("\nConfig file: ./inst/", config_filename, "\n\n"), file = logfile_build, append = TRUE)

if (deliverable_test) cat(paste("\nPROTECT deliverable settings\n"), file = logfile_build, append = TRUE)
if (impute_sims != "none") {
  cat(paste("Impute missing data in simulations:", impute_sims, "\n"), file = logfile_build, append = TRUE)
  cat(paste("including extension of timeseries by up to", impute_nyrs, "years\n"), file = logfile_build, append = TRUE)
}
cat( paste("\nEnsemble subset:", ensemble_subset,"\n"), file = logfile_build, append = TRUE)
cat(paste( "MODELS:", paste(model_list, collapse = ", "), "\n"), file = logfile_build, append = TRUE)
cat(paste("\nDate range of simulations to be used:",
          first_year,"-", final_year, "\n"),
    file = logfile_build, append = TRUE)
cat(paste("\nEmulator type:", emulator_type), file = logfile_build, append = TRUE)
cat(paste("\nEmulator covariance:", emulator_covar), file = logfile_build, append = TRUE)
if (emulator_type == "deepgp") cat(paste("\nN_MCMC:", N_mcmc, "\n"), file = logfile_build, append = TRUE)
if (emulator_type == "laGP") {
  cat("\nlaGP scaling: ", laGP_scaling, "\n", file = logfile_build, append = TRUE)
  cat("laGP method: ", laGP_method, "\n", file = logfile_build, append = TRUE)
  cat("laGP nugget prior: ", laGP_nugget_prior, "\n", file = logfile_build, append = TRUE)
}
cat(paste("\nValidation type:", validation_type), file = logfile_build, append = TRUE)
cat(paste("\nHistory matching before emulation:", do_history_match), file = logfile_build, append = TRUE)
if (do_history_match) cat(paste("\nModel discrepancy for h.m.:", scale_mod_err_sel), file = logfile_build, append = TRUE)
cat(paste("\nSVD variance threshold:", scree_thresh), file = logfile_build, append = TRUE)
if (i_s == "GLA") {
  cat(paste("\nGlacier observations:", glacier_data), file = logfile_build, append = TRUE)
  cat(paste("\nGlacier completion thresholds for OGGM, GO:",
            complete_thresh[["OGGM"]], complete_thresh[["GO"]]),
      file = logfile_build, append = TRUE)
}
cat(paste("\nPlot level:", plot_level), file = logfile_build, append = TRUE)
cat("\n", file = logfile_build, append = TRUE)

#' ## Glacier maximum contributions
# Get glacier cap --------

if (i_s == "GLA") glacier_cap <- emulandice2::get_glacier_cap(reg)

# Calibration dates --------
#' ## Baseline and calibration dates

# Ice sheets: IMBIE3 (Otosaka et al., in review)
# Glaciers: default is GlaMBIE (The GlaMBIE Team, 2025)
#           and additional option is Hugonnet et al. (2021)

# Start and end of calibration period
# NOTE: all emulation and calculations in emulandice2 build stage are done relative to cal_start
# Predict stage then re-baselines by subtracting a later year, e.g. 2005

# Antarctica
if (i_s == "AIS") {

  # Use start of IMBIE, or earliest start date of all but 1 model to avoid imputing too much
  # Checked % original sims too
  if ( final_year > 2150) { cal_start <- 1979 # Long-term: Kori, PISM and IMAUICE start in 1950/1; BISICLES 2007 but impute
  } else cal_start <- 1996 # Short-term: Kori, PISM and UFEMISM 1980/81; Kori 1981; CISM 1996; Elmer/ice 2000 so impute

  # End of IMBIE is 31/12/2023 which means using next year for annual values
  cal_end <- 2024

}

# Greenland
if (i_s == "GIS") {

  # Use start of IMBIE, or earliest start date of all but 1 model to avoid imputing too much
  # Checked % original sims too
  if (final_year > 2100) { # Long-term: CISM starts in 1960
    cal_start = 1972
  } else {
    cal_start = 1990 # Short-term: IMAUICE 1960; GISM 1990; Elmer/Ice 1995
  }

  # End of IMBIE is 31/12/2023 which means using next year for annual values
  cal_end <- 2024
}

# Glaciers
if (i_s == "GLA") {

  # Both GlaMBIE and Hugonnet et al. start in 2000
  cal_start <- 2000

  # End of datasets - use next year as date because annual values
  if (glacier_data == "GlaMBIE") cal_end <- 2024 # GlaMBIE is newer: to 31/12/2023
  if (glacier_data == "Hugonnet") cal_end <- 2020 # Hugonnet is to 31/12/2019
}

# Over-ride if wanting to run deliverable options (obsolete)
if (deliverable_test) {
  cal_start <- 2000
  cal_end <- 2020
}

# IMBIE3 start dates check in case moved
if (i_s == "AIS") stopifnot( cal_start >= 1979 )
if (i_s == "GIS") stopifnot( cal_start >= 1972 )

# GLAMBIE and Hugonnet start date check
if (i_s == "GLA") stopifnot( cal_start >= 2000 )

# All datasets end about the same time
stopifnot( cal_end <= 2024 )
if (i_s == "GLA" && glacier_data == "Hugonnet") stopifnot( cal_end <= 2020 )

# Construct emulated time series
if (deliverable_test) {
  break_yr <- NA
  proj_start <- cal_start + nyrs
  years_em <- seq( from = proj_start, by = nyrs, to = final_year )
} else {
  break_yr <- 2030 # end of annual frequency for emulation
  years_em <- c( (cal_start + 1):break_yr-1, seq( from = break_yr, by = nyrs, to = final_year ))
}

# BISICLES tests xxx drop a couple
#years_em <- years_em[ -(length(years_em)-1) ]
#years_em <- years_em[ -(length(years_em)-2) ]
#years_em <- years_em[ -(length(years_em)-4) ]

# Basic check not done something daft with timeslices
stopifnot(2100 %in% years_em)

# End of calibration is in projection period, so check we are predicting this year
stopifnot(cal_end %in% years_em)

if (deliverable_test) {
  cat( paste("Predicting every", nyrs, "years from",
             years_em[1], "to", years_em[length(years_em)], "\n"),
       file = logfile_build, append = TRUE)
} else {
  cat( paste("Predicting annually from", years_em[1], "to", break_yr,
             "then every", nyrs, "years to", years_em[length(years_em)], "\n"),
       file = logfile_build, append = TRUE)
}
cat(paste("with respect to year", cal_start, "\n"), file = logfile_build, append = TRUE)

N_ts <- length(years_em)
cat(paste("Timeslices:", N_ts, "\n"), file = logfile_build, append = TRUE)

#' ## Leave-one-out (LOO) validation choices
validation_years <- c( cal_end, 2050, 2100, 2150, 2200, 2300)
cat(paste("Validation years:", paste(validation_years, collapse = ",")), "\n", file = logfile_build, append = TRUE)

# (Checks these years are emulated later)

#' ## Emulation input choices

# Emulator settings ------------------------------------------------------------
#_______________
cat("\nEMULATOR INPUTS:\n", file = logfile_build, append = TRUE)

# Switch for GSAT means or SVD (only summary for now)
temp_input <- "mean"
stopifnot(temp_input == "mean")

# // Temps ------------------------------------------------------------


# Number of years to average over
# e.g. setting 10 with temps_list = 2300 and temps_baseline_end = 2010
# gives decadal mean 2291-2300 relative to 2015-2024
tn <- config::get("temp_nyrs", file = config_file)
N_temp_yrs <- ifelse(!is.null(tn), tn, 10) # Default if not specified

# GSAT timeslices for ice_design
tl <- config::get("temp_end_years", file = config_file)

# First is end of baseline, so need at least two in list if specifying
stopifnot(is.null(tl[1]) || (!is.null(tl[1]) && length(tl) > 1))

# Not too many, to avoid linear combinations (esp bad for fixed climate GIS) or overfitting
# Timeslices are dropped below if request shorter projections e.g. to 2150 only

# Set from config file
if (!is.null(tl[2])) {
  temps_baseline_end <- tl[1]
  temps_list <- tl[2:length(tl)]
} else {

  # Default if not specified; baseline is 2001-2010 i.e. before SSPs start
  temps_list <- seq(2010, 2300, by = N_temp_yrs)
  temps_baseline_end <- temps_list[1]
  temps_list <- temps_list[-1]

  # Old defaults
  #if (i_s == "AIS") temps_list <- 2300
  if (i_s == "GIS") {
    #  temps_list <- 2100
    if (deliverable_test) temps_list <- 2100
  }
  if (i_s == "GLA") {
    #  temps_list <- c(2100, 2300)
    if (deliverable_test) temps_list <- 2300
  }
}

# Save first year of baseline for some checks/outputs
temps_baseline_start <- min(emulandice2:::gsat_window_years(temps_baseline_end, N_temp_yrs))

# Type of anomalies: relative to baseline, or relative to each successive timeslice?
# Set default to baseline for now
temp_anom_type <- config::get("temp_anom_type", file = config_file)
temp_type <- ifelse(!is.null(temp_anom_type), temp_anom_type, "baseline")
stopifnot( temp_type %in% c("baseline", "relative"))


cat(paste("GSAT anomaly type:", temp_type, "\n"), file = logfile_build, append = TRUE)
cat(paste("GSAT baseline:", temps_baseline_start, "-", temps_baseline_end, "\n"), file = logfile_build, append = TRUE)
cat(paste("Initial GSAT timeslice final year(s):", paste(temps_list, collapse = ","), "\n"), file = logfile_build, append = TRUE)
if (max(temps_list) > final_year) {
  cat("GSAT timeslice(s) extend beyond ice model simulation: dropping\n", file = logfile_build, append = TRUE)
  temps_list <- temps_list[ temps_list <= final_year ]
  if (length(temps_list) == 0) temps_list <- final_year
  cat(paste("New GSAT input timeslice(s):", paste(temps_list, collapse = ","), "\n"), file = logfile_build, append = TRUE)
}
cat(paste("GSAT period:", N_temp_yrs, "years\n"), file = logfile_build, append = TRUE)

# xxx Can use this elsewhere! e.g. plot_design.R instead of reconstructing
temps_list_names <- paste0("GSAT_", temps_list)

# // Ice model params ----------------------------------------------------------

# Ice model parameters for ice_design


if (i_s == "AIS") {

  ice_cont_list_model <- list()
  ice_factor_list_model <- list()

  # Kori: all
  ice_cont_list_model[["Kori"]] <- c("heat_flux_PICO", "heat_flux_Plume", "heat_flux_ISMIP6_local",
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
                                       "overturning_PICO") } # probably already added
  #                                       "tillwater_decay_rate",
  #                                       "eff_fraction_overburden_pressure")
  #  }

  # PISM different resolution between the two (8km and 16km)
  if ( ensemble_subset == "all_forced" && final_year <= 2200 ) {
    ice_cont_list_model[["PISM"]] <- c(ice_cont_list_model[["PISM"]],
                                       "resolution")
  }


  # CISM
  ice_cont_list_model[["CISM"]] <- c( "resolution",
                                      "heat_flux_ISMIP6_nonlocal",
                                      "heat_flux_ISMIP6_nonlocal_slope")

  # Local is only varied in CISM for runs to 2100
  # but these are imputed to 2150 if impute_sims = "extend"
  if (final_year == "2100" || impute_sims == "extend") ice_cont_list_model[["CISM"]] <- c(ice_cont_list_model[["CISM"]],
                                                                                          "heat_flux_ISMIP6_local")
  ice_factor_list_model[["CISM"]] <- c("melt_param", "sliding_law")

  # Elmer/Ice
  ice_cont_list_model[["ElmerIce"]] <- c("heat_flux_PICO", "sliding_exponent")

  # BISICLES
  ice_cont_list_model[["BISICLES"]] <- "heat_flux_ISMIP6_nonlocal"
  ice_factor_list_model[["BISICLES"]] <- c("shelf_collapse", "sliding_law")

  # IMAUICE
  ice_factor_list_model[["IMAUICE"]] <- "GIA"

  # UFEMISM - ensemble also changes init method but not used in emulator
  ice_cont_list_model[["UFEMISM"]] <- "resolution"

  # Combine model lists
  ice_cont_list <- NA
  ice_factor_list <- NA

  for (mm in model_list) {
    if (length(ice_cont_list_model[[mm]]) > 0) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[[mm]])
    if (length(ice_factor_list_model[[mm]]) > 0) ice_factor_list <- c(ice_factor_list, ice_factor_list_model[[mm]])
  }

  # If both models present, can also include this
  # as they use different values
  # i.e. this covers GCM-forced 2300
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

  drop_list <- NA

  # 2300: terms confounding with models and/or small % of ensemble
  if (final_year > 2200) drop_list <- c("init_atmos", "init_ocean", "GIA", "shelf_collapse", "sliding_law", # factors
                                        "overturning_PICO" ) # continuous

  if ( length(drop_list) > 1 || (length(drop_list) == 1 && ! is.na(drop_list)) ) {
    cat("Dropping these inputs:", paste(drop_list, collapse = ", "), "\n",
        file = logfile_build, append = TRUE)
    ice_cont_list <- ice_cont_list[ ! ice_cont_list %in% drop_list ]
    ice_factor_list <- ice_factor_list[ ! ice_factor_list %in% drop_list ]
  }

}

# Continuous and categorical (factor) model inputs
if (i_s == "GIS") {

  # xxx Drop SP_climate column - not used and has missing
  # xxx Ignoring retreat_hist for now

  # Individual model lists
  # No factors for GISM
  # xxx Make init_yrs continuous?
  ice_factor_list_model <- list()
  ice_factor_list_model[["CISM"]] <- c("thermodyn", "RCM_init")

  # init_yrs and elev_feedback are redundant in 2300 ensemble, so only add to 2100
  # (if redundant in that ensemble, emulator will now stop with rank deficiency complaint)
  if (final_year <= 2100) {
    ice_factor_list_model[["CISM"]] <- c(ice_factor_list_model[["CISM"]], "init_yrs", "elev_feedback")
  }

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

  # GO
  ice_cont_list_model[["GO"]] <- c("prec_corr_factor", "ice_albedo",
                                   "temp_sens", "psi_constant","trans",
                                   "t_tip", "t_phase")

  # Combine
  ice_cont_list <- NA
  if ("GloGEM" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["GloGEM"]])
  if ("OGGM" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["OGGM"]])
  if ("GO" %in% model_list) ice_cont_list <- c(ice_cont_list, ice_cont_list_model[["GO"]])
  ice_cont_list <- ice_cont_list[-1]
  ice_cont_list <- unique( ice_cont_list )

  # Factors
  ice_factor_list <- NA

  # Ensemble is for any setup differences, e.g.:
  # For OGGM, forcing uses reanalysis 2000-2020 and parameter uses GM
  # For GloGEM, forcing parameters are regional means over glaciers but
  # parameter ensemble has same value everywhere

  # If using both ensembles xxx check if this should be 2150 if/when imputing??
  # could just check dataset for ensemble == forcing xxx

  if (final_year <= 2100) ice_factor_list <- c(ice_factor_list, "ensemble")

  # Multiple models
  if (length(model_list) > 1) ice_factor_list <- c(ice_factor_list, "model")

  # Drop initial NA if added any
  if ( length(ice_factor_list > 1) ) ice_factor_list <- ice_factor_list[-1]

  #  if (length(model_list) > 1) { ice_factor_list <- "model"
  #  } else ice_factor_list <- NA

}

cat(paste("\nContinuous inputs:", paste(ice_cont_list, collapse = " "), "\n"), file = logfile_build, append = TRUE)

# XXX add check that not NA or single value column
# e.g. sliding for CISM is always Schoof

# Combine lists
# assumes always have at least 1 continuous
# but factor might be NA
# Save whether any factors for other uses
if (anyNA(ice_factor_list)) {
  include_factors <- FALSE
  ice_param_list <- ice_cont_list
} else {
  include_factors <- TRUE
  ice_param_list <- c(ice_cont_list, ice_factor_list)
}

if (include_factors) {
  cat(paste("Factors:", paste(ice_factor_list, collapse = " "), "\n"), file = logfile_build, append = TRUE)
}

#' ## Emulator details

if (emulator_type == "statGP") {

  # Could set to FALSE if want to check for inert inputs
  lower_bound <- TRUE # RobustGaSP default = TRUE
  alpha = NA

  # Matern
  if (emulator_covar %in% c("matern_5_2", "matern_3_2")) kernel <- emulator_covar

  # Power exponential
  if (emulator_covar == "pow_exp_01") {
    kernel <- "pow_exp"
    alpha = 0.1
  }
  if (emulator_covar == "pow_exp_10") {
    kernel <- "pow_exp"
    alpha = 1.0
  }
  if (emulator_covar == "pow_exp_19") {
    kernel <- "pow_exp"
    alpha = 1.9 # default for pow_exp
  }
  if (emulator_covar == "pow_exp_20") {
    kernel <- "pow_exp"
    alpha = 2.0
  }

  stopifnot(kernel %in% c("pow_exp", "matern_5_2", "matern_3_2"))

}

if (emulator_type == "deepgp") {
  # Placeholder if I want to set matern smoothness parameter later
}

# Plot choices ------------------------------------------------------------
#' ## Plot choices

# plot_level set earlier from YAML: 2 for exhaustive (default if not set),
# 1 for some; 0 for none

# Write validation and SA RData file for nice replotting later
write_sa <- TRUE

# Sub-sample to plot; exclude any dates not predicted by emulator
yy_plot <- c(as.character(cal_end),"2100", "2150", "2200", "2300")
yy_plot <- yy_plot[ yy_plot %in% years_em ]

# Same for LOO timeslices
validation_years <- validation_years[ validation_years %in% years_em ]
if (length(validation_years) == 0 ) warning("None of the requested validation years are in predictions")

# Match short and full scenario names for plots
# xxx ADD RCPs?
scen_name <- list()
for (scen in scenario_list) {
  tmp <- strsplit(scen, split="")[[1]]
  scen_name[[scen]] <- paste( c(tmp[1:4], "-", tmp[5], ".", tmp[6]), collapse = "")

  if (scen %in% c("SSP534-over", "SSP534-over-recon")) scen_name[[scen]] <- "SSP5-3.4-OS"
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
  sle_lim[["2250"]] <- c(-250, 800); sle_inc[["2250"]] <- 10
  sle_lim[["2300"]] <- c(-300, 1000); sle_inc[["2300"]] <- 20
}

if (i_s == "GIS") {
  sle_lim[[as.character(cal_end)]] <- c(-4, 8); sle_inc[[as.character(cal_end)]] <- 0.5
  sle_lim[["2050"]] <- c(-1, 10); sle_inc[["2050"]] <- 0.5
  sle_lim[["2100"]] <- c(-20, 40); sle_inc[["2100"]] <- 1
  sle_lim[["2150"]] <- c(-50, 100); sle_inc[["2150"]] <- 2
  sle_lim[["2200"]] <- c(-100, 220); sle_inc[["2200"]] <- 5
  sle_lim[["2250"]] <- c(-150, 350); sle_inc[["2250"]] <- 5
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
    sle_lim[[as.character(cal_end)]] <- c(-3, 4); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-2, 1.5*glacier_cap); sle_inc[["2050"]] <- 0.5
    sle_lim[["2100"]] <- c(-2, 1.5*glacier_cap); sle_inc[["2100"]] <- 0.5
    sle_lim[["2150"]] <- c(-2, 2*glacier_cap); sle_inc[["2150"]] <- 0.5
    sle_lim[["2200"]] <- c(-2, 2*glacier_cap); sle_inc[["2200"]] <- 1
    sle_lim[["2300"]] <- c(-10, 2*glacier_cap); sle_inc[["2300"]] <- 0.2 # -70, 1 XXX why was it so big ??
  }
  if (reg == "RGI19") {
    sle_lim[[as.character(cal_end)]] <- c(-0.1, 1); sle_inc[[as.character(cal_end)]] <- 0.1
    sle_lim[["2050"]] <- c(-10, glacier_cap); sle_inc[["2050"]] <- 0.5
    sle_lim[["2100"]] <- c(-10, 1.1*glacier_cap); sle_inc[["2100"]] <- 0.5
    sle_lim[["2150"]] <- c(-10, 1.3*glacier_cap); sle_inc[["2150"]] <- 0.5
    sle_lim[["2200"]] <- c(-10, 1.4*glacier_cap); sle_inc[["2200"]] <- 1
    sle_lim[["2300"]] <- c(-10, 1.5*glacier_cap); sle_inc[["2300"]] <- 1
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

AR6_rgb[["RCP26"]] <- rgb(23, 60, 102, maxColorValue = 255)
AR6_rgb[["RCP85"]] <- rgb(149, 27, 30, maxColorValue = 255)

# 60% transparency
AR6_rgb_med <- list()
alpha_med <- 153
AR6_rgb_med[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_med)

AR6_rgb_med[["RCP26"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_med[["RCP85"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_med)

# 10% transparency; 20% = 51
AR6_rgb_light <- list()
alpha_light <- 51
AR6_rgb_light[["SSP119"]] <- rgb(0, 173, 207, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP126"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP245"]] <- rgb(247, 148, 32, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP370"]] <- rgb(231, 29, 37, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["SSP585"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_light)

AR6_rgb_light[["RCP26"]] <- rgb(23, 60, 102, maxColorValue = 255, alpha = alpha_light)
AR6_rgb_light[["RCP85"]] <- rgb(149, 27, 30, maxColorValue = 255, alpha = alpha_light)

# Overshoot colour
# According to
# https://pyam-iamc.readthedocs.io/en/stable/tutorials/ipcc_colors.html
AR6_rgb[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255)
AR6_rgb_med[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_light[["SSP534-over"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_light)

AR6_rgb[["SSP534-over-recon"]] <- rgb(146, 57, 122, maxColorValue = 255)
AR6_rgb_med[["SSP534-over-recon"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_med)
AR6_rgb_light[["SSP534-over-recon"]] <- rgb(146, 57, 122, maxColorValue = 255, alpha = alpha_light)

# ________________----
# START ------------------------------------------------------------
#' ## START


#' # Load and process data

#' ## Load observations
# Load data: -------------------------------------------------------------------
## observations -------------------------------------------------------------------

# Needs to be before select_sims for history matching filtering of glaciers
obs_data <- emulandice2::load_obs()

# Sanity check observations
cat("\n\nFirst and last sea level observations (cm SLE, 1 s.d.):\n", file = logfile_build, append = TRUE)
cat(sprintf("%s: %.4f +/- %.4f\n%s: %.4f +/- %.4f\n",
            obs_data[1,"Year"], obs_data[1,"SLE"], obs_data[1,"SLE_sd"], # first
            obs_data[nrow(obs_data),"Year"],
            obs_data[nrow(obs_data),"SLE"], obs_data[nrow(obs_data),"SLE_sd"] ), # last
    file = logfile_build, append = TRUE)

# Calculate and print total sea level contribution over observational period
# Hugonnet is already the total change
if (i_s == "GLA" && glacier_data == "Hugonnet") {
  obs_period <- "2000-01-01_2020-01-01"
  obs_change <- obs_data[obs_data$Year == obs_period, "SLE"]
  obs_err <- obs_data[obs_data$Year == obs_period, "SLE_sd"]
} else {
  stopifnot( cal_start %in% obs_data[, "Year"] )
  stopifnot( cal_end %in% obs_data[, "Year"] )
  obs_change <- obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
  obs_err <- obs_data[obs_data$Year == cal_end,"SLE_sd"]
  obs_period <- paste0( cal_start, "-", cal_end)
}
cat(sprintf("\nObserved sea level change (cm SLE, %s):\n", obs_period), file = logfile_build, append = TRUE)
cat(sprintf("%.4f +/- %.4f cm SLE (+/- 1 s.d. obs error)\n", obs_change, obs_err),
    file = logfile_build, append = TRUE)
cat(sprintf("%.4f +/- %.4f cm SLE (+/- 3 s.d obs error)\n", obs_change, 3*obs_err), file = logfile_build, append = TRUE)

#' ## Load climate and ice simulations
## climate sims ---------------------------------------------------------------------

# GET CLIMATE SIMULATIONS

# Returns CSV file data
climate_csv <- emulandice2::load_sims(variable = "climate")

# Fill any missing final year values (all rows in climate file - not very efficient)
climate_data <- impute_climate(climate_csv)

# XXX DROP ROWS NOT IN CSV HERE I THINK

# Calculate climate change timeslice(s) e.g. GSAT_2100 for emulator input(s)
# Also for all rows in climate data - not very efficient
# Option to add ensemble mean for each SSP for missing forcings
# (for imputing to extend ice simulations later)
impute_gcms <- ifelse(impute_sims == "extend", TRUE, FALSE)

# Calculate summary means
# nrows(climate_data) x {scenario, GCM, anom}
temps_data <- emulandice2::calc_temps_gcms(climate_data, mean_impute = impute_gcms)

# For GIS post-2100, repeat with fixed climate forcings
# No need to set mean_impute, because this is already filling in [?] xxx check
if ( i_s == "GIS" && final_year > 2100) {
  if (temp_input == "mean") climate_data_fixed <- impute_climate(climate_csv, construct_fixed = TRUE)
  temps_data_fixed <- emulandice2::calc_temps_gcms(climate_data_fixed)
}

# For AIS Kori and PISM, splice historical NorESM1
# before 2015 (GCM-forced) and optionally also before 1980 (RCM-forced)
if ( i_s == "AIS" && any(model_list %in% c("Kori", "PISM")) && temps_baseline_start < 2015L) {
  if (temp_input == "mean") climate_data_spliced_late <- impute_climate(climate_csv, construct_spliced = 2015)
  temps_data_spliced_late <- emulandice2::calc_temps_gcms(climate_data_spliced_late, mean_impute = impute_gcms)

  if (temps_baseline_start < 1980L) {
    if (temp_input == "mean") climate_data_spliced_early <- impute_climate(climate_csv, construct_spliced = 1980)
    temps_data_spliced_early <- emulandice2::calc_temps_gcms(climate_data_spliced_early, mean_impute = impute_gcms)
  }
}

## ice sims ---------------------------------------------------------------------
# GET ICE SIMULATIONS

# Also converts all units to cm SLE
ice_data <- emulandice2::load_sims(variable = "ice", source = i_s, region = reg) # ice dataset

# Index of first column with name format of yXXXX
ice_file_yr_start_col <- suppressWarnings( myind <- min(which( nchar(names(ice_data)) == 5
                                                               & substr(names(ice_data), start = 1, stop = 1) == "y"
                                                               & !is.na(as.numeric(substr(names(ice_data), start = 2, stop = 5)) ) ) ) )

# Check requested years are within file year range from these columns
stopifnot(first_year >= as.numeric(substr(names(ice_data)[ ice_file_yr_start_col ], 2, 5)) &&
            final_year <= as.numeric(substr(names(ice_data)[ length(names(ice_data)) ], 2, 5)) )

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

# Do second selection for glaciers using values of SLE change
# xxx no longer works because sims are not in same units as obs
if (deliverable_test) {
  if (i_s == "GLA") {
    ice_data <- emulandice2::select_sims("history_match")
  }
}


# Match climate ---------------------------------------------------------------

# Get corresponding climate change(s) (match by GCM + scenario)
# This is currently nrows(ice_data) x {scenario, GCM, temps}
temps <- emulandice2::match_gcms(ice_data, temps_data, mean_impute = impute_gcms)


## get fixed ---------------------------------------------------------------

# For GIS post-2100, get fixed climate forcing change(s)
# and overwrite into rows of temps with fixed_date = 2100
if (i_s == "GIS" && final_year > 2100) {

  # Index of simulations forced with fixed climate from 2100 (column flag in dataset)
  fixed_ind <- ice_data$fixed_date == 2100 & !is.na(ice_data$fixed_date)

  cat("\nNow try matching again after reconstructing fixed post-2100 forcings in dataset\n",
      file = logfile_build, append = TRUE)

  # Summary means
  temps_fixed <- emulandice2::match_gcms(ice_data, temps_data_fixed)
  temps[ fixed_ind, ] <- temps_fixed[ fixed_ind, ]

  cat("Fixed forcings: wrote GSAT fixed climate post-2100 for", sum(fixed_ind), "sims\n",
      file = logfile_build, append = TRUE)

}

## get spliced ---------------------------------------------------------------

# For AIS, splice Kori and PISM forcings, as they use NorESM1-M before 1980/2015

# GCM-forced Kori/PISM: late splice historical (cut at 2015)
if ( i_s == "AIS" && any(model_list %in% c("Kori", "PISM")) && temps_baseline_start < 2015L) {

  cat("\nNow match again after splicing NorESM1 historical to use for Kori/PISM\n",
      file = logfile_build, append = TRUE)

  if (exists("temps_data_spliced_late")) {
    late_ind <- ice_data$model %in% c("Kori","PISM") & ice_data$forcing_type == "GCM"
    temps_late <- match_gcms(ice_data, temps_data_spliced_late, mean_impute = impute_gcms)
    temps[late_ind, ] <- temps_late[late_ind, ]
    cat("\nNorESM splice: overwrote pre-2015 forcing with NorESM1-M for",
        sum(late_ind), "GCM-forced Kori/PISM sims\n",
        file = logfile_build, append = TRUE)
  }

  # RCM-forced: early splice (cut at 1980)
  if (temps_baseline_start < 1980L && exists("temps_data_spliced_early")) {

    early_ind <- ice_data$model %in% c("Kori","PISM") & ice_data$forcing_type == "RCM"
    temps_early <- match_gcms(ice_data, temps_data_spliced_early, mean_impute = impute_gcms)
    temps[early_ind, ] <- temps_early[early_ind, ]
    cat("NorESM splice: overwrote pre-1980 forcing with NorESM1-M for",
        sum(early_ind), "RCM-forced Kori/PISM sims\n",
        file = logfile_build, append = TRUE)
  }
}

# Drop scenario and GCM columns: just keep climate column(s)
# TODO: drop = FALSE and remove row vs column checks where possible
temps <- temps[ , -(1:2) ]

# Make numeric
if (length(temps_list) == 1) { temps <- as.numeric(temps)
} else temps <- apply(temps, 2, as.numeric)

# Find ice simulations that have climate forcing (just last timeslice if multiple)
if ( length(temps_list) == 1 ) { sim_index <- !is.na(temps)
} else sim_index <- !is.na(temps[, length(temps_list)])

# Report how many dropped (if any)
cat("\nDropped", sum(!sim_index), "simulations due to lack of forcing (missing, or unable to impute, final timeslice(s))\n",
    file = logfile_build, append = TRUE)

# Keep only these in both ice and climate data
ice_data <- ice_data[ sim_index, ]
if ( length(temps_list) == 1) { temps <- temps[ sim_index ]
} else temps <- temps[ sim_index, ]

# Double-check again
if (anyNA(temps) || !all(is.finite(as.matrix(temps)))) {
  stop("Missing data in GSAT timeslice(s): may need to drop forcings or change GSAT choices e.g. baseline")
}

# END OF ICE SIMULATION SELECTION
N_sims <- dim(ice_data)[1]

cat(paste("\nDATA SELECTION: using", N_sims, "ice simulations for",
          i_s, reg, "\n"), file = logfile_build, append = TRUE)

cat("\nOf which:", "\n", file = logfile_build, append = TRUE)
for (mm in model_list) {
  cat( paste0(mm, ": ", length( ice_data[ice_data$model == mm, 1] )), "\n",
       file = logfile_build, append = TRUE)
}

# Check some simulations found!
stopifnot(N_sims > 0)


# Retrieve Greenland fixed post-2100 climate forcings
if ( i_s == "GIS" && final_year > 2100) {

  # Update fixed_ind with final ice simulation dataset
  fixed_ind <- ice_data$fixed_date == 2100 & !is.na(ice_data$fixed_date)

  cat(paste("\nNumber of simulations forced with fixed post-2100 climate:",
            nrow(ice_data[ fixed_ind, ]), "\n"),
      file = logfile_build, append = TRUE)

  # TODO: could rewrite following using Cursor method of selection
  # used for spliced_late and spliced_early forcings below
  match_sims_fixed <- unique(ice_data[ fixed_ind, c("scenario", "GCM")])

  # Also select in climate_data for full time series forcing plot
  climate_data_test <- apply(match_sims_fixed, 1, function(x) { # as in match_gcms()

    # For each row in forcings list, get climate timeseries
    climate_data_fixed[ climate_data_fixed$GCM == x[ "GCM" ]
                        & climate_data_fixed$scenario == x[ "scenario"], ]
  })

  # Ugh: convert list to numeric matrix...
  tmp <- matrix(0.0, nrow = dim(match_sims_fixed)[1], ncol = dim(climate_data_fixed)[2] - 2)
  for ( cc in 1:length(climate_data_test)) {
    tmp[ cc, ] <- as.numeric(unlist(climate_data_test[[cc]][, 3:dim(climate_data_fixed)[2]]))
  }
  colnames(tmp) <- colnames(climate_data_fixed[ , 3:dim(climate_data_fixed)[2]])

  # Overwrite old climate_data_fixed with selected this subset and scenario/GCM columns
  climate_data_fixed <- cbind(match_sims_fixed, tmp)

}

# Get final list of scenarios and GCMs to write to text and plot forcings (not very efficient!)
if ( i_s == "GIS" && final_year > 2100) {
  match_sims <- unique(ice_data[ !fixed_ind, c("scenario", "GCM")])
} else if ( i_s == "AIS" && temps_baseline_start < 2015L) {
  splice_ind <- ice_data$model %in% c("Kori", "PISM") & (ice_data$forcing_type == "GCM" |
                                                           (temps_baseline_start < 1980L & ice_data$forcing_type == "RCM"))
  match_sims_spliced <- unique(ice_data[ splice_ind, c("scenario", "GCM")])
  match_sims <- unique(ice_data[ !splice_ind, c("scenario", "GCM")])
} else {
  match_sims <- unique(ice_data[ , c("scenario", "GCM")])
}

# If any original GSAT simulations to plot (as opposed to GIS fixed or AIS spliced)
if (nrow(match_sims) > 0L) {
  climate_data_test <- apply(match_sims, 1, function(x) { # as in match_gcms()

    # For each row in forcings list, get climate timeseries
    climate_data[ climate_data$GCM == x[ "GCM" ]
                  & climate_data$scenario == x[ "scenario"], ]
  })

  # Ugh: convert list to numeric matrix...
  tmp <- matrix(0.0, nrow = nrow(match_sims), ncol = ncol(climate_data) - 2)

  for ( cc in 1:length(climate_data_test)) {
    if (nrow(climate_data_test[[cc]][, 3:dim(climate_data)[2]]) == 0 ) {
      cat(paste("\nWaarning: cannot find forcing number",cc,"in CSV file:\n"),
          file = logfile_build, append = TRUE)
    } else {
      tmp[ cc, ] <- as.numeric(unlist(climate_data_test[[cc]][, 3:dim(climate_data)[2]]))
    }
  }
  colnames(tmp) <- colnames(climate_data[ , 3:dim(climate_data)[2]])

  # Overwrite old climate_data with selected this subset and scenario/GCM columns
  climate_data <- cbind(match_sims, tmp)

} else {

  # None found so drop rows
  # e.g. Kori/PISM-only use climate_data_spliced_*, so original climate_data will be empty
  climate_data <- climate_data[0, , drop = FALSE]
}

# Also restrict spliced plot tables to forcings used by current ice_data
# (refiltered later after history matching)
if (i_s == "AIS" && exists("climate_data_spliced_late")) {
  late_ind <- ice_data$model %in% c("Kori", "PISM") & ice_data$forcing_type == "GCM"
  if (any(late_ind)) {
    late_keys <- unique(ice_data[late_ind, c("scenario", "GCM"), drop = FALSE])
    climate_data_spliced_late <- merge(late_keys, climate_data_spliced_late,
                                       by = c("scenario", "GCM"), sort = FALSE)
  } else {
    climate_data_spliced_late <- climate_data_spliced_late[0, , drop = FALSE]
  }
}

if (i_s == "AIS" && exists("climate_data_spliced_early")) {
  early_ind <- ice_data$model %in% c("Kori", "PISM") & ice_data$forcing_type == "RCM"
  if (any(early_ind)) {
    early_keys <- unique(ice_data[early_ind, c("scenario", "GCM"), drop = FALSE])
    climate_data_spliced_early <- merge(early_keys, climate_data_spliced_early,
                                        by = c("scenario", "GCM"), sort = FALSE)
  } else {
    climate_data_spliced_early <- climate_data_spliced_early[0, , drop = FALSE]
  }
}

# Output number of timeseries kept
n_fixed <- if (exists("climate_data_fixed")) nrow(climate_data_fixed) else 0L
n_late  <- if (exists("climate_data_spliced_late")) nrow(climate_data_spliced_late) else 0L
n_early <- if (exists("climate_data_spliced_early")) nrow(climate_data_spliced_early) else 0L

cat(paste("\nUsing these forcings\n",
          "Original (including those imputed from ensemble mean):", nrow(climate_data), "\n",
          "Fixed after 2100 (only for GIS):", n_fixed, "\n",
          "Spliced with NorESM1-M before 2015 (only for AIS with Kori/PISM):", n_late, "\n",
          "Spliced with NorESM1-M before 1980 (ditto):", n_early, "\n",
          "TOTAL:", nrow(climate_data) + n_fixed + n_late + n_early, "\n\n"),
    file = logfile_build, append = TRUE)

cat("Using these",nrow(match_sims),"original forcings:\n", file = logfile_build, append = TRUE)
ms <- match_sims[ sort(match_sims[,"scenario"], index.return = TRUE)$ix, ]
for( mm in seq_len(nrow(ms)) ) {
  cat( unlist(ms[mm, c("scenario", "GCM")]), "\n", file = logfile_build, append = TRUE)
}

if (exists("match_sims_fixed")) {
  cat(paste("\nUsing these",nrow(match_sims_fixed),"forcings fixed from 2100:\n"),
      file = logfile_build, append = TRUE)
  ms <- match_sims_fixed[ sort(match_sims_fixed[,"scenario"], index.return = TRUE)$ix, ]
  for( mm in 1:dim(ms)[1]) {
    cat( unlist(ms[mm, c("scenario", "GCM")]), "\n", file = logfile_build, append = TRUE)
  }
}

if (exists("match_sims_spliced")) {
  cat(paste("\nUsing these",nrow(match_sims_spliced),"forcings spliced before 1980 and/or 2015:\n"),
      file = logfile_build, append = TRUE)
  ms <- match_sims_spliced[ sort(match_sims_spliced[,"scenario"], index.return = TRUE)$ix, ]
  for( mm in 1:dim(ms)[1]) {
    cat( unlist(ms[mm, c("scenario", "GCM")]), "\n", file = logfile_build, append = TRUE)
  }
}

# Matrix checks ------

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
if (anyNA( ice_data[ , ice_param_list ] )) stop("NAs found in ice_data columns to use as inputs in emulation: please drop/fix")

# COMBINE CLIMATE FORCING AND CONTINUUOUS ICE MODEL INPUTS INTO DESIGN MATRIX
ice_design <- as.matrix( data.frame(temps, ice_data[ ice_cont_list ]) )

# Add climate col names
colnames(ice_design)[ 1:length(temps_list) ] <- temps_list_names

# Create axis label for plots
GSAT_lab <- list()
for (tt in 1:length(temps_list_names)) {
  GSAT_lab[[temps_list_names[tt]]] <- paste0('Global mean temperature ',
                                             temps_list[tt]-N_temp_yrs+1,'-',temps_list[tt],
                                             ' rel. to ',temps_baseline_start,'-',temps_baseline_end,' (degC)')
}

# Factor processing: ---------------------------------------------------------------
#' ## Merging of similar small factor levels
# Based on knowledge of model similarity/difference, and flagged by % of ensemble (output below)
# As characteristics are very granular in dataset and should be grouped
# Differences between simulations go into model term or nugget

## factor level merging ---------------------------------------------------------------
if (i_s == "AIS" ) {

  # Longer timescales checked first
  if (final_year > 2150) {

    cat(paste("\nMerging some similar and/or rare factor levels (if blank then none):\n"), file = logfile_build, append = TRUE)

    if ("sliding_law" %in% ice_factor_list) {
      cat(paste("\nsliding_law was:",paste(unique(ice_data[,"sliding_law"]), collapse = " ")), file = logfile_build, append = TRUE)

      # Merge all sliding laws that have effective pressure dependence
      # (small numbers in ensemble, and similar response)
      # Based on conversations with Bill Lipscombe and Helene Seroussi
      ice_data[ ice_data$sliding_law == "power_law_Tsai", "sliding_law" ] <- "eff_pressure"
      ice_data[ ice_data$sliding_law == "Zoet-Iverson", "sliding_law" ] <- "eff_pressure"
      ice_data[ ice_data$sliding_law == "Coulomb_reg_300", "sliding_law" ] <- "eff_pressure"
      ice_data[ ice_data$sliding_law == "Coulomb_reg_50", "sliding_law" ] <- "eff_pressure"
      cat(paste("\nand is now:",paste(unique(ice_data[,"sliding_law"]), collapse = " "),"\n"), file = logfile_build, append = TRUE)
    }

    if ("GIA" %in% ice_factor_list) {

      # Merge 2 types of GIA in IMAUICE ensemble (still small fraction)
      cat(paste("\nGIA was:",paste(unique(ice_data[,"GIA"]), collapse = " ")), file = logfile_build, append = TRUE)
      ice_data[ ice_data$GIA == "3D_strong", "GIA" ] <- "3D"
      ice_data[ ice_data$GIA == "3D_weak", "GIA" ] <- "3D"
      cat(paste("\nand is now:",paste(unique(ice_data[,"GIA"]), collapse = " "),"\n"), file = logfile_build, append = TRUE)
    }

  }

}


## one-hot encoding ---------------------------------------------------------------
#' ## One-hot encoding of factors

ice_factor_values <- list()

# ADD FACTOR COLUMNS
if ( include_factors ) {

  # For each factor input
  for ( ff in ice_factor_list ) {

    cat(paste("\nFactor to add:", ff, "\n"), file = logfile_build, append = TRUE)

    # Get all factor level values present in the dataset
    ff_vals <- sort(unique(ice_data[ ,ff]))
    cat(paste("Levels:", length(ff_vals), "\n"), file = logfile_build, append = TRUE)

    # Count levels first to make the most common one the reference (more stable)
    ff_count <- sapply(ff_vals, function(fx) sum(ice_data[, ff] == fx))
    ff_ref <- ff_vals[which.max(ff_count)]

    cat(paste("Adding",length(ff_vals) - 1,"dummy variables with reference value:", ff_ref, "\n"), file = logfile_build, append = TRUE)

    # Loop over levels
    for ( vv in ff_vals ) {

      # Warn if fraction of ensemble with this level is < 5% (can be unstable)
      frac_level <- 100.0 * sum(ice_data[, ff] == vv) / nrow(ice_data)

      cat(sprintf("Factor:level %s:%s (%.1f%% of ensemble)\n", ff, vv, frac_level),
          file = logfile_build, append = TRUE)
      if (frac_level < 5.0) cat(paste0("Warning: small fraction of ensemble is ",vv,
                                       " - consider merging this with other level(s)\n"),
                                file = logfile_build, append = TRUE)

      # Drop reference level to avoid collinearity
      if (vv == ff_ref) {
        cat(sprintf("- reference value\n"),
            file = logfile_build, append = TRUE)
        next
      }

      # Name of dummy variable column is factor:level
      cat(sprintf("- generating dummy variable column for %s:%s\n", ff, vv),
          file = logfile_build, append = TRUE)

      # Set to 1 or 0
      ice_design <- cbind(ice_design, ifelse(ice_data[, ff] == vv, 1, 0 ) )
      colnames(ice_design)[dim(ice_design)[2]] <- paste(ff, vv, sep = ":")

    }

    # Save to use for prior: put reference first as all designs expect this
    # (because did originally use alphabetical ordering to choose reference)
    ice_factor_values[[ff]] <- c(ff_ref, ff_vals[ff_vals != ff_ref])

  } # factors loop
}

# Save list of continuous inputs in design
input_cont_list <- c(temps_list_names, ice_cont_list)

# Save list of ice inputs: not ice_param_list but expanded dummy versions
ice_dummy_list <- NA

if (include_factors) {
  ice_dummy_list <- colnames(ice_design)[ ! colnames(ice_design) %in% input_cont_list]
  ice_all_list <- c( ice_cont_list, ice_dummy_list)
} else {
  ice_all_list <- ice_cont_list
}


#' #' ## Scale inputs for emulator
#' # Scale inputs ---------------------------------------------------------------

# cat("\nOriginal ranges of inputs:\n", file = logfile_build, append = TRUE)
# for (cc in 1:dim(ice_design)[2]) {
#   cat( paste(colnames(ice_design)[cc], min(ice_design[,cc]), "to",
#              max(ice_design[,cc]), "\n"), file = logfile_build, append = TRUE)
# }
#
# # Scale columns of continuous parameters (climate and ice model)
# cat("\nCentre and scale continuous inputs (mean = 0, s.d. = 1)\n", file = logfile_build, append = TRUE)
# ice_design_scaled_cont <- scale(ice_design[, input_cont_list])
#
# # Store scaling to use later for prior
# inputs_centre <- attr(ice_design_scaled_cont,"scaled:center")
# inputs_scale <- attr(ice_design_scaled_cont,"scaled:scale")
#
# # Fill back into original design
# ice_design_scaled <- ice_design
# ice_design_scaled[, input_cont_list] <- ice_design_scaled_cont
#
# cat("\nNew ranges of inputs after scaling:\n", file = logfile_build, append = TRUE)
# for (cc in 1:dim(ice_design_scaled)[2]) {
#   cat( paste(colnames(ice_design_scaled)[cc], min(ice_design_scaled[,cc]), "to",
#              max(ice_design_scaled[,cc]), "\n"), file = logfile_build, append = TRUE)
# }

# Make sure scenario list only includes those of simulations
scenario_list <- scenario_list[ scenario_list %in% unique(ice_data[,"scenario"]) ]
#cat(paste("Scenario list:",paste(scenario_list, collapse = ","), "\n"), logfile_build, append = TRUE)

#save.image(file="~/PROTECT/emulandice2/sims.RData")


#' # Plot simulations
# Plot: sims -----------------------------------------------------------------------

cat("\nPlot simulator projections\n", file = logfile_build, append = TRUE)

# Plot raw simulations
if (plot_level > 0) {

  pdf( file = paste0( plotdir, out_name, "_DESIGN_ORIG.pdf"),
       width = 9, height = 5)
  emulandice2::plot_designs("sims", plot_level)
  dev.off()

  pdf( file = paste0( plotdir, out_name, "_SIMS_ORIG.pdf"),
       width = 9, height = 5)

  # TODO: remove vertical line at cal_end using plot_obs argument
  emulandice2::plot_timeseries("sims", plot_level)

  # Raw simulations (original baseline, no imputing)
  emulandice2::plot_scatter("sims", "none", plot_level, plot_obs = FALSE)

  # TODO: add sims option to plot SLE histograms
  # if doing anything not already covered by plot_design
  # emulandice2::plot_distributions("sims", plot_level)

  dev.off()


}

# Impute missing years ---------------------------------------------------------------

# Save simulations as sims_data, because we will replace ice_data with imputed after this
# Not currently used
# TODO: Save sims_data to .RData if wanting to replot nicely later
sims_data <- ice_data

if (impute_sims != "none") {

  # Impute data (take from end of calibration period ta avoid calibrating imputed)
  cat( paste0("\nRequested impute simulations with SVD: ",impute_sims,"\n"),
       file = logfile_build, append = TRUE)

  # Use SVD to impute missing projection years
  # impute_sims sets whether none, minor fills, or major extensions forward/back
  ice_data_proj <- ice_data[ , paste0("y", years_em) ]
  num_miss <- is.na(ice_data_proj)

  miss_sims <- apply(ice_data_proj, 1, function(x) {
    ifelse( length(x[ is.na(x) ]) > 0, TRUE, FALSE)
  })

  cat( paste("\nNumber of simulations with missing values:",
             sum(miss_sims),"\n"), file = logfile_build, append = TRUE)
  cat( paste("\nImputing",sum(num_miss),"simulation values\n"), file = logfile_build, append = TRUE)

  if (sum(num_miss) > 0) {

    ice_data_impute <- emulandice2::SVDimpute( as.matrix(ice_data_proj),
                                               pmin = 1 - 1E-5)

    pdf( file = paste0( plotdir, out_name, "_impute.pdf"),
         width = 9, height = 5)

    # All data
    matplot(years_em, t(ice_data_impute), type = "n",
            col = grey(0.1, 0.1), lty = 1, xlab = "Year", ylab = "Sea level contribution (cm SLE)",
            main = ice_name)

    # Imputed values (where original had NA)
    # If only imputed 1 simulation, don't transpose
    if (sum(num_miss) == 1) {
      matlines(years_em, ice_data_impute[ miss_sims, ],
               type = "l", col = "red", lty = 1, lwd = 0.5)
    } else {
      matlines(years_em, t(ice_data_impute[ miss_sims, ]),
               type = "l", col = "red", lty = 1, lwd = 0.5)
    }

    # Simulated values
    matlines(years_em, t(ice_data_proj[ miss_sims, ]),
             type = "l", col = "black", lty = 1, lwd = 0.5)

    dev.off()

    # Zoom AIS historical xxx change ylim so can plot for any
    if (i_s == "AIS" ) {

      pdf( file = paste0( plotdir, out_name, "_impute_zoom.pdf"), width = 9, height = 5)

      matplot(years_em, t(ice_data_impute), type = "n", xlim = c(1970,2100),
              ylim = c(-20,50), col = grey(0.1, 0.1), lty = 1,
              xlab = "Year", ylab = "Sea level contribution (cm SLE)",  main = ice_name)
      abline(v=2014, lwd=0.5, lty=3)

      # As above
      if (sum(num_miss) == 1) {
        matlines(years_em, ice_data_impute[ miss_sims, ],
                 type = "l", col = "red", lty = 1, lwd = 0.5)
      } else {
        matlines(years_em, t(ice_data_impute[ miss_sims, ]),
                 type = "l", col = "red", lty = 1, lwd = 0.5)
      }
      matlines(years_em, t(ice_data_proj[ miss_sims, ]),type = "l", col = "black", lty = 1, lwd = 0.5)

      dev.off()
    }

    # if any were missing
  } else {

    # else return original
    ice_data_impute <- ice_data_proj
  }
}

# i.e. use imputed data for all future simulation data and plots
ice_data[ , paste0("y", years_em)] <- ice_data_impute

# Rebaseline by subtracting value in year cal_end
ice_data <- emulandice2::calculate_sle_anom(ice_data, baseline=cal_start)

# History matching ---------------------------------------------------------------

# History matching with observations - returns row index
if (do_history_match) {

  nroy_sel <- emulandice2::select_sims("history_match")
  #save.image(file=paste0(rdatadir, out_name, "_sims_impute.RData"))

  # Select for everything... xxx re-order code to improve?
  # Emulator data:
  ice_data <- ice_data[ nroy_sel, , drop = FALSE]

  # Temps and design:
  if ( length(temps_list) == 1) { temps <- temps[ nroy_sel ]
  } else temps <- temps[ nroy_sel, , drop = FALSE ]
  ice_design <- ice_design[ nroy_sel, , drop = FALSE]

  # LOO xxx but could rewrite to use nrow(ice_data) in do_LOO.R
  N_sims <- nrow(ice_data)

  # Not needed (temporary or not used): ice_data_impute, ice_data_proj,
  # sims_data, sim_index
  # imputing things

  cat(paste("\nFINAL DATA SELECTION: using", N_sims, "ice simulations for",
            i_s, reg, "\n"), file = logfile_build, append = TRUE)

  cat("\nOf which:", "\n", file = logfile_build, append = TRUE)
  for (mm in model_list) {
    cat( paste0(mm, ": ", length( ice_data[ice_data$model == mm, 1] )), "\n",
         file = logfile_build, append = TRUE)
  }

}

# Ice sheet regions ------------------------------------------------------------

# Fixed fractions from final selected simulations (NROY if history matching ran)
if (i_s %in% c("AIS", "GIS") && do_regions) {

  rf <- emulandice2::calc_region_fracs()
  region_names <- rf$region_names
  region_fracs <- rf$region_fracs

}

# Keep original forcings for plotting only if they match to final simulations
# Uses same !fixed / !splice rules as pre-HM, but on final ice_data (i.e. NROY if HM ran).
if (do_history_match) {

  # Index fixed vs non-fixed forcings; same for spliced
  if (i_s == "GIS" && final_year > 2100L) {
    fixed_ind <- ice_data$fixed_date == 2100 & !is.na(ice_data$fixed_date)
    native_keys <- unique(ice_data[!fixed_ind, c("scenario", "GCM"), drop = FALSE])
  } else if (i_s == "AIS" && temps_baseline_start < 2015L) {
    splice_ind <- ice_data$model %in% c("Kori", "PISM") & (
      ice_data$forcing_type == "GCM" |
        (temps_baseline_start < 1980L & ice_data$forcing_type == "RCM")
    )
    native_keys <- unique(ice_data[!splice_ind, c("scenario", "GCM"), drop = FALSE])
  } else {
    native_keys <- unique(ice_data[, c("scenario", "GCM"), drop = FALSE])
  }

  # If original (non-fixed, non-spliced) forcings are associated with final dataset, keep these
  if (nrow(native_keys) > 0L) {
    climate_data <- merge(native_keys, climate_data,
                          by = c("scenario", "GCM"), sort = FALSE)
  } else {
    climate_data <- climate_data[0, , drop = FALSE]
  }
}

# Keep fixed forcings for plotting only if they match to final simulations
if (i_s == "GIS" && final_year > 2100 && exists("climate_data_fixed")) {

  # Final set of simulations with fixed forcings
  fixed_ind <- ice_data$fixed_date == 2100 & !is.na(ice_data$fixed_date)

  # If any simulations associated with fixed forcings, keep in climate_data_fixed
  if (any(fixed_ind)) {
    fix_keys <- unique(ice_data[fixed_ind, c("scenario", "GCM"), drop = FALSE])
    climate_data_fixed <- merge(fix_keys, climate_data_fixed,
                                by = c("scenario", "GCM"), sort = FALSE)
  } else {
    # Keep the object structure, but drop all rows so they are not plotted
    climate_data_fixed <- climate_data_fixed[0, , drop = FALSE]
  }
}

# As above, but for spliced forcings
if (i_s == "AIS" && temps_baseline_start < 2015L) {

  if (exists("climate_data_spliced_late")) {
    late_ind <- ice_data$model %in% c("Kori","PISM") & ice_data$forcing_type == "GCM"
    if (any(late_ind)) {
      late_keys <- unique(ice_data[late_ind, c("scenario", "GCM"), drop = FALSE])
      climate_data_spliced_late <- merge(late_keys, climate_data_spliced_late,
                                         by = c("scenario", "GCM"), sort = FALSE)
    } else {
      climate_data_spliced_late <- climate_data_spliced_late[0, , drop = FALSE]
    }
  }
  if (exists("climate_data_spliced_early")) {
    early_ind <- ice_data$model %in% c("Kori","PISM") & ice_data$forcing_type == "RCM"
    if (any(early_ind)) {
      early_keys <- unique(ice_data[early_ind, c("scenario", "GCM"), drop = FALSE])
      climate_data_spliced_early <- merge(early_keys, climate_data_spliced_early,
                                          by = c("scenario", "GCM"), sort = FALSE)
    } else {
      climate_data_spliced_early <- climate_data_spliced_early[0, , drop = FALSE]
    }
  }

}


#' # Plot final dataset
# Plot: final data -----------------------------------------------------------------------

#save.image(file=paste0(rdatadir, out_name, "_sims_impute.RData"))

# Re-plot for imputed - now same baseline as observations
if (plot_level > 0) {

  pdf( file = paste0( plotdir, out_name, "_DESIGN_FINAL.pdf"),
       width = 9, height = 5)
  emulandice2::plot_designs("sims", plot_level)
  dev.off()

  pdf( file = paste0( plotdir, out_name, "_SIMS_FINAL.pdf"),
       width = 9, height = 5)
  emulandice2::plot_timeseries("sims", plot_level)
  # Need to tidy and fix [Note: copied from SIM.pdf]
  emulandice2::plot_scatter("sims", "none", plot_level) # shown in SA plots as black dots (not always RCPs)
  # Need to add sims option to plot SLE histograms
  #emulandice2::plot_distributions("sims", plot_level) # xxx check if doing anything or covered by plot_design...
  dev.off()

}

# ________________----
#' # EMULATION
# EMULATION  ------------------------------------------------------------

# FULL DATASET
# Will train emulator with this full dataset ice_data (full LOO)
# or random/ordered subset selected below (LOO subset or TVT)

# Inputs
Xtrain <- ice_design

# Outputs: includes any imputed values (and ALL_validation.R)
Ytrain <- ice_data[ , paste0("y", years_em) ]

# Train emulators with:

# If validation_type == "loo":
# 1. Select N = 1000 here, if N_ensemble > 1000 and doing LOO
# 2. Otherwise, skip and use N = N_ensemble

# If validation_type == "tvt":
# 3. N =~ 1000, if dataset large and using normal GP
# 4. N = 70% of N_ensemble, if dataset medium-large OR if GP can cope with large dataset (e.g. laGP)
# i.e. select non-random sample and reserve 30% / remaining for testing


#' # Select data subset
# Select data subset if TVT ------------------------------------------------------------

# SUBSET DATA FOR TRAINING: 70% of total, or 70% of N_max_em
# Samples a balance of factor levels, not just random

# Will be set to TRUE later if subset taken
train_subset <- FALSE

# Indices for train and test dataset rows used in TVT
train_set <- NULL
test_set <- NULL

# Only do this selection if not using LOO validation later
# except if reproducing deliverable, which set limit of 1000 for all
if ( validation_type != "loo" | # case 3,4
     ( deliverable_test && validation_type == "loo" && nrow(ice_data) > 1000L )) { # case 1

  # Get full dataset design
  # Yes, really! Not emulator inputs, because full list includes e.g. GCM, SSP etc
  # which is good for sampling GSAT and noisy ice responses to GCMs for given GSAT
  # So this uses columns that may be ignored everywhere else
  Xraw <- ice_data[, ice_param_list_full]

  # Make into nice data frame with factors
  make_factor <- function(x) {
    x[is.na(x)] <- "NA"
    factor(x)
  }

  Xraw <- lapply(Xraw, function(x) {
    if(is.character(x)) {
      make_factor(x)
    } else {
      x
    }}) |> as.data.frame()

  # Run through factor list to pick up any numeric or T/F factors
  for (cc in colnames(Xraw)) {
    if (cc %in% ice_factor_list & !is.factor(Xraw[, cc])) Xraw[, cc] <- make_factor(Xraw[, cc])
  }

  cat( paste("\nMax sample size for training:",N_max_em,"\n"),
       file = logfile_build, append = TRUE)

  # Number of simulations to train with
  # If limit set for large dataset: trains with N_max_em at most
  if ( !is.na(N_max_em)) {

    # Case 3/4 switch: case 4 unless too big, then case 3
    target_size <- round( min(0.7 * nrow(ice_data), N_max_em) )

  } else {
    # If no limit set (e.g. for GP that can handle large data): train with 70% of full ensemble
    # Case 4 with no limit
    target_size <- round(0.7 * nrow(ice_data))

  }

  # Or subset for LOO in deliverable_test mode
  if (deliverable_test && validation_type == "loo" && nrow(ice_data) > 1000L) {
    target_size <- 1000L
  }

  cat( paste("\nSelecting",target_size,"simulations for training:\n"),
       file = logfile_build, append = TRUE)

  # Case 1: deliverable used random sample subset for LOO
  if ( deliverable_test && validation_type == "loo" && nrow(ice_data) > 1000L ) {
    cat( paste("- random sample\n"),
         file = logfile_build, append = TRUE)
    train_set <- sort(sample(nrow(ice_data), target_size))

  } else {

    # Order ensemble using all factors in original dataset file
    # to pick the most informative simulations with respect to the factor levels
    cat( paste("- ordered sample\n"),
         file = logfile_build, append = TRUE)

    # Output factors
    cat("\n** Factor levels being used for ordering:\n", file = logfile_build, append = TRUE)
    for (jj in which(sapply(Xraw, is.factor))) {
      cat(paste0("\t", names(Xraw)[ jj ], ":\n"), file = logfile_build, append = TRUE)
      cat(paste0("\t", paste(levels(Xraw[[ jj ]]), collapse = ", "), "\n"), file = logfile_build, append = TRUE)
    }

    ## Reorder dataset design to make sure factor levels well-sampled at start of list
    # (simple random if no factors)
    reordered <- reorder_rows(Xraw, frontLoad = TRUE)

    # Improved method: select first N_subset of rows for training
    train_set <- reordered[ 1:target_size ]

    # These will be used for validation later
    test_set <- reordered[-(1:target_size)]

  }

  # Apply random/ordered selection to raw design (just for checking), and inputs and outputs
  Xraw_sub <- Xraw[ train_set, ]
  Xtrain <- Xtrain[ train_set, ]
  Ytrain <- Ytrain[ train_set, ]
  train_subset <- TRUE

  # Factor levels in training data - all factors, not just emulated
  cat("\n** Factor levels present in training subset:\n", file = logfile_build, append = TRUE)
  for (jj in which(sapply(Xraw_sub, is.factor))) {
    cat(paste0("\t", names(Xraw_sub)[ jj ], ":\n"), file = logfile_build, append = TRUE)
    cat(paste0("\t", paste(levels(Xraw_sub[[ jj ]]), collapse = ", "), "\n"), file = logfile_build, append = TRUE)
  }

} # if not LOO (or if sampling for deliverable_test LOO)

# Start log file -----
# Start log file

emu_log_file <- paste0(outdir, out_name,"_", emulator_type, ".log")
cat("______________________________________\n", file = emu_log_file)
cat("EMULATOR LOG FILE\n\n", file = emu_log_file, append = TRUE)
cat("MAIN EMULATOR:\n", file = emu_log_file, append = TRUE)
cat("______________________________________\n", file = emu_log_file, append = TRUE)

# Drop temp inputs -----

# Need three colons for these functions because emulator_build.R is outside package and function is not exported

# If multiple GSAT timeslices in design, drop any that are too highly correlated with others
if (length(temps_list_names) > 1) {

  cat("\nDropping any highly correlated GSAT timeslices...\n",
      file = logfile_build, append = TRUE)
  temps_list_names_drop <- emulandice2:::drop_temps(Xtrain)

  # If some were dropped
  if ( sum(is.na(temps_list_names_drop)) == 0 ) {

    keep <- setdiff(colnames(Xtrain), temps_list_names_drop)
    out <- emulandice2:::sync_design_inputs(keep_names = keep,
                                            Xtrain = Xtrain,
                                            ice_design = ice_design,
                                            temps = temps,
                                            temps_list = temps_list,
                                            temps_list_names = temps_list_names,
                                            ice_cont_list = ice_cont_list,
                                            ice_dummy_list = ice_dummy_list,
                                            ice_all_list = ice_all_list,
                                            input_cont_list = input_cont_list)

    Xtrain <- out$Xtrain
    ice_design <- out$ice_design
    temps <- out$temps
    temps_list <- out$temps_list
    temps_list_names <- out$temps_list_names
    ice_cont_list <- out$ice_cont_list
    ice_dummy_list <- out$ice_dummy_list
    ice_all_list <- out$ice_all_list
    input_cont_list <- out$input_cont_list
  }

  cat("\nKeeping",length(temps_list_names),"GSAT timeslices: ", paste(temps_list_names, collapse=" "),
      "\n", file = logfile_build, append = TRUE)

}

#' ## Relative GSAT timeslices
# Relative temps ---------------------------------------------------------------

# If relative timeslices (no need if only one)
if (temp_type == "relative" && length(temps_list) > 1) {

  # Get GSAT colnames (intersect keeps temps_list order)
  gsat_cols <- intersect(temps_list_names, colnames(ice_design))

  # Replace ice_design temps with relative i.e. difference with previous timeslice not baseline
  ice_design[ , gsat_cols ] <- emulandice2:::gsat_abs_to_relative(ice_design[, gsat_cols, drop = FALSE])

  # Overwrite temps too (matrix of 2 or more columns, due to temps_list length check)
  temps <- emulandice2:::gsat_abs_to_relative(temps)

  # Update GSAT labels
  for (tt in seq_along(temps_list_names)) {

    if (tt == 1) next
    win <- paste0(temps_list[tt] - N_temp_yrs + 1, "-", temps_list[tt])
    prev <- paste0(temps_list[tt - 1] - N_temp_yrs + 1, "-", temps_list[tt - 1])

    GSAT_lab[[temps_list_names[tt]]] <- paste0(
      "Global mean temperature ", win, " minus ", prev, " (degC)")
  }

}

#' ## Scale inputs for emulator
# Scale inputs ---------------------------------------------------------------
# Scale FULL dataset (not just Xtrain, which for TVT are rows selected for training)

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

# Fill continuous parameters back into original design
ice_design_scaled <- ice_design
ice_design_scaled[, input_cont_list] <- ice_design_scaled_cont

cat("\nNew ranges of inputs after scaling:\n", file = logfile_build, append = TRUE)
for (cc in 1:dim(ice_design_scaled)[2]) {
  cat( paste(colnames(ice_design_scaled)[cc], min(ice_design_scaled[,cc]), "to",
             max(ice_design_scaled[,cc]), "\n"), file = logfile_build, append = TRUE)
}

# Now grab Xtrain columns (and if TVT, rows) of ice_design_scaled
if (train_subset) {
  Xtrain <- ice_design_scaled[ train_set, colnames(Xtrain), drop = FALSE]
} else {
  Xtrain <- ice_design_scaled[ , colnames(Xtrain), drop = FALSE]
}

#' ## Check inputs for emulator
# Check inputs ---------------------------------------------------------------

# Check main design matrix rank and conditioning: returns 0 if good, > 0 if fails test(s)
# Note any other use of make_emu as in LOO does not use this - could move inside make_emu?
check_X <- emulandice2:::check_design(Xtrain)

# Returns an integer
cat("\nNumber of design matrix checks failed:", check_X, "\n",
    file = logfile_build, append = TRUE)

# For strict checking, must pass all tests (not needed for RobustGaSP, but may be wise precaution)
# stopifnot(check_X == 0)

# Check we have the same number of rows in design and output matrices
stopifnot(nrow(Xtrain) == nrow(Ytrain))

# Sims only for testing: stop just before emulation
if ( read_sims_only) {
  print("Stopping before emulation")
  quit(save = "no")
}


# Build: make_emu -----
# BUILD EMULATOR
# Writes emu obj into .RData workspace file later for running in FACTS
# Note this call is repeated in do_LOO.R

print("Building emulator...")


# Flag for whether we are coming from main.R or emulator_build.R
is_build <- TRUE

# If change anything here, also change in do_loo()
# Design continuous inputs are scaled
# scree_thresh is set earlier from YAML (default 0.999; GIS 2300 increased threshold if unset)

if (temp_input == "mean") { # TODO: delete temp_input - legacy of trying SVD for GSAT
  emu_mv <- emulandice2::make_emu( designX = as.matrix(Xtrain),
                                   responseF = as.matrix(Ytrain),
                                   thresh = scree_thresh)

  # save.image(file="~/PROTECT/emulandice2/make_emu.RData")

  # Get number of inputs in trend
  emu_inputs <- emu_mv( Xtrain[ 1, ], type = "var")$inputs

  # Drop inert inputs not used for emulation from design, for plots etc
  if (length(emu_inputs) < length(colnames(Xtrain))) {

    cat("\nEmulator dropped", length(colnames(Xtrain)) - length(emu_inputs),
        "inert inputs from design:", setdiff(colnames(Xtrain), emu_inputs), "\n",
        file = logfile_build, append = TRUE)

    out <- emulandice2:::sync_design_inputs(keep_names = emu_inputs,
                                            Xtrain = Xtrain,
                                            ice_design = ice_design,
                                            temps = temps,
                                            temps_list = temps_list,
                                            temps_list_names = temps_list_names,
                                            ice_cont_list = ice_cont_list,
                                            ice_dummy_list = ice_dummy_list,
                                            ice_all_list = ice_all_list,
                                            input_cont_list = input_cont_list,
                                            inputs_centre = inputs_centre,
                                            inputs_scale = inputs_scale,
                                            ice_design_scaled = ice_design_scaled)
    Xtrain <- out$Xtrain
    ice_design <- out$ice_design
    temps <- out$temps
    temps_list <- out$temps_list
    temps_list_names <- out$temps_list_names
    ice_cont_list <- out$ice_cont_list
    ice_dummy_list <- out$ice_dummy_list
    ice_all_list <- out$ice_all_list
    input_cont_list <- out$input_cont_list
    inputs_centre <- out$inputs_centre
    inputs_scale <- out$inputs_scale
    ice_design_scaled <- out$ice_design_scaled

    # Factor values - do separately so don't drop nominal
    print(ice_factor_values)
    ice_factor_values_new <- ice_factor_values

    # Loop through factors
    for (ff in names(ice_factor_values)) {

      # Loop through levels of this factor, starting at 2 as first value is nominal
      for (vv in 2:length(ice_factor_values[[ff]])) {

        # If factor:level is not in inputs
        factor_level <- paste(ff, ice_factor_values[[ff]][vv], sep=":")

        if ( ! factor_level %in% emu_inputs ) {
          print(paste("factor:level", factor_level, "is not in emulator inputs" ))
          # Drop from values
          ice_factor_values_new[[ff]] <- ice_factor_values_new[[ff]][ ice_factor_values_new[[ff]] != ice_factor_values_new[[ff]][vv] ]
          #    print(ice_factor_values_new[[ff]])
        }
      }

      # If all levels of a factor dropped, drop from factor list for designs/plots
      # and from names of factor value list
      # 2 for reference + any remaining xxx is this OK
      if (length(ice_factor_values_new[[ff]]) == 2 && anyNA(ice_factor_values_new[[ff]])) {
        print(paste("Drop", ff, "from factor list because all dummy (non-reference) levels dropped"))
        ice_factor_list <- ice_factor_list[ ice_factor_list != ff]
        ice_factor_values_new <- ice_factor_values_new[ names(ice_factor_values_new) != ff ]
        stopifnot(names(ice_factor_values_new) == ice_factor_list)
      }
    } # factor loop

    ice_factor_values <- ice_factor_values_new

    cat("\nSo now we have",length(emu_inputs),"inputs as follows:\n", file = logfile_build, append = TRUE)
    cat(paste(colnames(Xtrain), collapse = " "),"\n", file = logfile_build, append = TRUE)

  } else {
    cat("\nEmulator dropped no inert inputs\n", file = logfile_build, append = TRUE)
  }

} # temp_input == "mean"


# ________________----
# SENSITIVITY ANALYSIS ------------------------------------------------------------


cat("______________________________________\n", file = emu_log_file, append = TRUE)
cat("EMULATOR: predict main effects\n", file = emu_log_file, append = TRUE)
cat("______________________________________\n", file = emu_log_file, append = TRUE)

#' # Predict for SA designs
# Design: main effects ---------------------------------------------------------

if (temp_input == "mean") {

  #' ## Main effects
  # Main effects (i.e. one-at-a-time design for sensitivity analysis)
  design_sa <- emulandice2::load_design_to_pred("main_effects", 100L)

  cat(paste("\nPredict for main effects:\n"), file = logfile_build, append = TRUE)
  cat(paste(names(design_sa), collapse = " "), "\n", file = logfile_build, append = TRUE)

  # Predict: overwrite object
  myem <- list()
  for (input in names( design_sa )) {

    cat(paste("\nMain effects:",input,"\n"), file = logfile_build, append = TRUE)

    cat(paste("Range:", min(design_sa[[input]][, input]), "-",
              max(design_sa[[input]][, input]),
              "\n"), file = logfile_build, append = TRUE)

    design_sa_scaled_cont <- scale(design_sa[[input]][ , input_cont_list],
                                   center = inputs_centre,
                                   scale = inputs_scale )

    design_sa_scaled <- as.data.frame( design_sa[[input]] )
    design_sa_scaled[ , input_cont_list ] <- design_sa_scaled_cont

    if (temp_input == "mean") myem[[input]] <- emulandice2::emulator_predict( design_sa_scaled, forcing_prior = "mean")
  }

  # save.image(file="~/PROTECT/emulandice2/MEFF.RData")

  #' ## Uniform temperature prior

  # Design: uniform --------------------------------------------------------------

  # Design "unif_temps" makes projections using uniform priors for GSAT with same ranges as sims
  # a better comparison than using FaIR projected distributions for each SSP

  cat("______________________________________\n", file = emu_log_file, append = TRUE)
  cat("EMULATOR: predict uniform prior\n", file = emu_log_file, append = TRUE)
  cat("______________________________________\n", file = emu_log_file, append = TRUE)

  design_pred <- emulandice2::load_design_to_pred("unif_temps", N_unif)

  cat("\nPredict for uniform temp designs:\n", file = logfile_build, append = TRUE)

  for (scen in scenario_list) {

    cat(paste("\nScenario with uniform priors:",scen,"\n"), file = logfile_build, append = TRUE)

    design_pred_scaled_cont <- scale(design_pred[[scen]][ , input_cont_list],
                                     center = inputs_centre,
                                     scale = inputs_scale )
    design_pred_scaled <- as.data.frame( design_pred[[scen]]  )
    design_pred_scaled[ , input_cont_list] <- design_pred_scaled_cont

    if (temp_input == "mean") myem[[scen]] <- emulandice2::emulator_predict( design_pred_scaled, forcing_prior = "mean" )

  }

  # save.image(file="~/PROTECT/emulandice2/unif_temps.RData")

  # Sample emu uncertainty ----------------------------------------------------------------------
  projections <- list()

  # Want to see unif_temps final projections (samples with uncertainty) for validation
  for (scen in scenario_list) {
    projections[[scen]] <- emulandice2::emulator_uncertainty(myem[[scen]])
  }

} # if temp_input == mean


# Plot: SA -----------------------------------------------------

if (temp_input == "mean") {

  # Plot sensitivity analysis
  if (plot_level > 0) {

    pdf( file = paste0( plotdir, out_name, "_MEFF.pdf"),
         width = 9, height = 5)
    emulandice2::plot_MEFF()
    dev.off()

    # SA uniform design: mean +/- 2 s.d.
    pdf( file = paste0( plotdir, out_name, "_SA_unif_mean.pdf"),
         width = 9, height = 5)
    emulandice2::plot_scatter("prior", "unif_temps", plot_level)
    dev.off()

    # SA uniform design: sample
    pdf( file = paste0( plotdir, out_name, "_SA_unif_final.pdf"),
         width = 9, height = 5)
    emulandice2::plot_scatter("posterior", "unif_temps", plot_level)
    dev.off()

  }
}

# ________________----
# VALIDATE ---------------------------------------------------------------------
#' # Validate

# New CSV file for diagnostics
csv_valid <- paste0(outdir, out_name, "_validation.csv")
if (file.exists(csv_valid)) file.remove(csv_valid)

# Validate: LOO ---------------------------------------------------------------------

# LOO VALIDATION: i.e. train on all-but-one, for validation
# Should only be used for small datasets

# Builds LOO emulators, and plots + keeps results for requested timeslices
if (validation_type == "loo") {

  cat("______________________________________\n", file = emu_log_file, append = TRUE)
  cat("EMULATOR: predict LOO\n", file = emu_log_file, append = TRUE)
  cat("______________________________________\n", file = emu_log_file, append = TRUE)

  cat(sprintf("\nLEAVE ONE OUT VALIDATION (N_k = %s)\n",ifelse(is.na(N_k), "all", N_k)), file = logfile_build, append = TRUE)

  # Test every N_k-th run (can be very slow)
  # xxx Improve: stratified by output value instead of every N_k
  if (temp_input == "mean") emu_loo <- emulandice2::do_loo( designX = as.matrix(Xtrain),
                                                            responseF = as.matrix(Ytrain),
                                                            N_k = N_k)

  # To store results
  loo_mean <- list()
  loo_sd <- list()
  wrong <- list()

  # Scenario labels aligned to Ytrain / do_loo rows
  if (is.null(train_set)) {
    loo_scenario <- as.character(ice_data$scenario)
  } else loo_scenario <- as.character(ice_data$scenario[train_set])
  stopifnot(length(loo_scenario) == nrow(Ytrain))

  # Loop over time slices to calculate metrics and plot_valid_metrics inputs
  for ( yy in validation_years) {

    cat("\nYear: ",yy,"\n", file = logfile_build, append = TRUE)

    yind <- paste0( "y", yy)

    # Get LOO predictions (in do_loo.R)
    loo_mean[[yind]] <- emu_loo$mean[ , yind]
    loo_sd[[yind]] <- emu_loo$sd[ , yind]
    stopifnot(length(loo_mean[[yind]]) == length(loo_scenario))

    # N_k selection of runs
    N_k_index <- !is.na(loo_mean[[yind]])

    # Which ones were within predicted intervals and which ones missed?
    # Needed by plot_valid_metrics (for highlighting misses)
    # (Also calculated as miss in calc_valid_metrics, but wrong can have NAs)
    wrong[[yind]] <- Ytrain[, yind] > (loo_mean[[yind]] + 2 * loo_sd[[yind]]) |
      Ytrain[, yind] < (loo_mean[[yind]] - 2 * loo_sd[[yind]])

    # Calculate validation metrics for all and also for
    # Helper function refactor by Cursor
    emulandice2::calc_valid_metrics(
      simulated = as.numeric(Ytrain[, yind]),
      emu_mean = as.numeric(loo_mean[[yind]]),
      emu_sd   = as.numeric(loo_sd[[yind]]),
      scenario = loo_scenario,
      row_mask = N_k_index,
      region = reg, year = yy, # For outputs
      csv_file = csv_valid, logfile = logfile_build
    )

  } # years

  if (plot_level > 0) {

    # Plot: LOO-------
    # Plot LOO results
    pdf( file = paste0( plotdir, out_name, "_VALID_METRICS.pdf"),
         width = 9, height = 5)
    emulandice2::plot_valid_metrics(valid_type = "LOO")
    dev.off()

    n_plot <- 10

    # 5 batches; or all for GIS 2300 (assuming still small-ish)
    nb <- if (i_s == "GIS" && final_year == 2300 && N_sims < 200) "all" else 5L
    pdf(file = paste0(plotdir, out_name, "_VALID_TIMESERIES.pdf"),
        width = 10, height = 5)
    emulandice2::plot_valid_timeseries("LOO", n_plot = n_plot, n_batches = nb)
    dev.off()

    # TODO: add VALID_SCATTER as for TVT?

  }

} # validation_type == "loo"

# Validate: TVT ---------------------------------------------------------------------
# Train and test validation

# TVT VALIDATION: training, validation and test (but currently use one test set, not two)
# i.e. predict left out set for validation
# Main method of validation

# Builds emulators on 70% of data (or 70% of N_max_em for large datasets),
# and plots + keeps results for requested timeslices
if (validation_type == "tvt") {

  cat("______________________________________\n", file = emu_log_file, append = TRUE)
  cat("EMULATOR: predict left-out test data\n", file = emu_log_file, append = TRUE)
  cat("______________________________________\n", file = emu_log_file, append = TRUE)

  cat(sprintf("\nTRAIN AND TEST VALIDATION (N = %i)\n", length(test_set)),
      file = logfile_build, append = TRUE)

  # Predict for all the original design points not in the training set
  # i.e. all but train_set rows
  # (values scaled before training set selection) using main emulator build (emu_mv)
  # This predicts full time series
  emu_test <- emulandice2::emulator_predict( ice_design_scaled[ test_set, ], forcing_prior = "mean" )

  # Get outputs for same test rows to validate the emulator with
  test_data <- ice_data[ test_set, paste0("y", years_em) ]

  # Unlike LOO, should be no missing data in these: i.e. values for all test sims
  test_mean <- list()
  test_sd <- list()
  wrong <- list()

  # Get scenarios
  tvt_scenario <- as.character(ice_data$scenario[test_set])
  stopifnot(length(tvt_scenario) == length(test_set))

  # For each validation year
  for ( yy in validation_years) {

    yind <- paste0("y", yy)

    test_mean[[yind]] <- emu_test$mean[ , yind]
    test_sd[[yind]] <- emu_test$sd[ , yind]
    stopifnot(length(test_mean[[yind]]) == length(tvt_scenario))

    # Misses
    # (Also calculated as miss in calc_valid_metrics, but wrong can have NAs)
    wrong[[ yind ]] <- test_data[ , yind] > ( test_mean[[yind]] + 2*test_sd[[yind]] ) |
      test_data[ , yind] < ( test_mean[[yind]]  - 2*test_sd[[yind]] )

    emulandice2::calc_valid_metrics(
      simulated = as.numeric(test_data[, yind]),
      emu_mean = as.numeric(test_mean[[yind]]),
      emu_sd   = as.numeric(test_sd[[yind]]),
      scenario = tvt_scenario,
      row_mask = NULL,
      region = reg,
      year = yy,
      csv_file = csv_valid,
      logfile = logfile_build
    )

  } # validation_years loop

  # Plot: TVT-------
  # Plot TVT results
  if (plot_level > 0) {

    n_plot <- 10
    nb <- if (i_s == "GIS" && final_year == 2300 && N_sims < 200) "all" else 5L

    pdf(file = paste0(plotdir, out_name, "_VALID_METRICS.pdf"),
        width = 9, height = 5)
    emulandice2::plot_valid_metrics(valid_type = "TVT")
    dev.off()

    pdf(file = paste0(plotdir, out_name, "_VALID_TIMESERIES.pdf"),
        width = 10, height = 5)
    emulandice2::plot_valid_timeseries("TVT", n_plot = n_plot, n_batches = nb)
    dev.off()

    pdf(file = paste0(plotdir, out_name, "_VALID_SCATTER.pdf"),
        width = 9, height = 5)
    emulandice2::plot_scatter("tvt", "validation", plot_level)
    dev.off()

  }

} # if tvt


# ________________----
# SAVE BUILD FILE ------------------------------------------------------------
#' # Save emulator build file

# SAVE EMULATOR BUILT FROM WHOLE ENSEMBLE
# and the rest of the workspace, at least for now
emu_file <- paste0(rdatadir, out_name, "_EMULATOR.RData")
sa_file <- paste0(rdatadir, out_name, "_SA.RData")

# Bit of duplication or unused
to_save <- c("climate_data", # CLIMATE MODEL SIMULATION DATA
             "ice_data", # ALL SELECTED ICE MODEL SIMULATION DATA
             "Xtrain", "Ytrain", # training data
             "obs_data", "obs_change", "obs_err", # OBSERVATION DATA (full data, total, and err in total)
             "inputs_preprocess", "inputs_ext", # Paths for package data
             "out_name", # General part of all output filenames
             "outdir", "logfile_build", # Used to write output in emulator function (see below)
             "deliverable_test", "do_regions", "impute_sims", # Analysis flags for info
             "model_list", # Could reconstruct from filename, but useful to have
             "scen_name", # Nicely formatted lookup name list of all scenarios looked for in datas
             "years_sim", # List of simulated years
             "ice_param_list_full", # Lists of all ice model simulated inputs
             "ice_design", # Simulation ensemble design, i.e. ice model input values
             "ice_cont_list", "ice_factor_list", "ice_all_list", # Lists of ice emulated inputs: continuous, factors, all
             "ice_dummy_list", "ice_factor_values", # Dummy column names and values for factor inputs
             "N_temp_yrs", # GSAT mean years; used in priors
             "temp_type", "temp_input", # GSAT anomaly type; using GSAT means or SVD xxx later obsolete if not using SVD
             "temps", "temps_baseline_end", "temps_list", "temps_list_names", # GSAT means and names used
             "input_cont_list", # List of all emulated continuous inputs, i.e. c(temps_list_names, ice_cont_list)
             "emulator_type",
             "emu_mv", # EMULATOR! function object
             "include_factors", "scree_thresh", # Are there any factors; scree threshold
             "years_em", "N_ts", # List and number of emulated years
             "inputs_centre", "inputs_scale", # Rescaling values for transforming params before/after emulation
             "first_year", "final_year", "cal_start", "cal_end", # Dates of data and calibration period
             "yy_plot", # Dates to plot
             "validation_type", "validation_years", # Save these for validation plotting
             "ice_name", # Nice ice source name for plots
             "GSAT_lab", # Nice plotting labels for GSAT means
             "sle_lim", "sle_inc", # Plotting ranges and increments (inc not used currently)
             "AR6_rgb", "AR6_rgb_light", "AR6_rgb_med" # Plotting colours
)

# Add extra bits for particular ice sources
# Climate forcings fixed post-2100
if ( i_s == "GIS" && final_year > 2100) {
  to_save <- c(to_save, "climate_data_fixed")
}
# Climate forcings spliced pre-2015
if ( i_s == "AIS" && temps_baseline_start < 2015L) {
  if (exists("climate_data_spliced_late")) to_save <- c(to_save, "climate_data_spliced_late")
  if (exists("climate_data_spliced_early")) to_save <- c(to_save, "climate_data_spliced_early")
}

# Glacier region maximum contributions
# Flag for GlaMBIE or Hugonnet
if (i_s == "GLA") to_save <- c(to_save, "glacier_cap", "glacier_data")

# Regional fractions
if (do_regions && i_s %in% c("AIS", "GIS")) to_save <- c(to_save, "region_names", "region_fracs")

# RobustGaSP emulator settings
if (emulator_type == "statGP") to_save <- c(to_save, "emulator_covar", "lower_bound", "kernel", "alpha")

# laGP settings
# Need to save these because build and predict are done together
if (emulator_type == "laGP") to_save <- c(to_save, "laGP_scaling", "laGP_method",
                                          "laGP_nugget_prior")
# If saving dgpsi serialised emulator objects, don't save to RData file
# xxx but not currently able to predict from main.R - keep code in case fixed later
#if (emulator_type == "dgpsi") {
#  to_save <- c(to_save, "Xtrain", "Ytrain") # save to re-call make_emu from main.R
#  to_save <- to_save[to_save != "emu_mv"] # don't save emulator
#}

save(list = to_save, file = emu_file)
cat(paste("\nSaved emulator object to RData file:",emu_file,"\n"), file = logfile_build, append = TRUE)

# Save SA file --------
# Save sensitivity analysis file (optional)

# SECOND FILE: validation and sensitivity analysis info

if (write_sa) {

  # Main effects and uniform designs; emulator SA predictions for these; validation years
  to_save_sa <- c( "design_sa", "design_pred", "myem", "validation_years", "validation_type")

  # LOO validation results
  if (validation_type == "loo") {
    to_save_sa <- c(to_save_sa, "loo_mean", "loo_sd", "emu_loo")
  }

  # Train and test validation results
  if ( validation_type == "tvt" ) {
    to_save_sa <- c(to_save_sa, "test_mean", "test_sd", "test_set", "emu_test" ) # full trajectory
  }

  # Validation wrong index has same name in both methods
  to_save_sa <- c(to_save_sa, "wrong")

  # Save file
  save(list = to_save_sa, file = sa_file)
  cat(paste("\nSaved validation and SA info to RData file:",sa_file,"\n"), file = logfile_build, append = TRUE)

}





