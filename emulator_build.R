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
#_______________________________________________________________________________

#' # Setup
# Setup ------------------------------------------------------------------------

# Get arguments from RScript
args <- commandArgs(TRUE)

# Defaults if no args set (used for testing and Markdown)
if (length(args) == 0) {

  warning("No arguments set - using defaults")
  i_s <- "AIS" #"GLA"
  reg_num <- 1 # 3
  final_year <- 2100 # 2300

} else {

  #' # Choose source and end year
  # Source and end year ----------------------------------------------------------

  # Ice source
  i_s <- args[1]

  # Region number (only used by glaciers for now)
  # xxx OGGM test PPE: regions 3,8,10,18 (will be replaced)
  reg_num <- as.numeric(args[2])

  # End year
  final_year <- as.numeric(args[3]) # if past 2100, applies model/ensemble selections later
  # Values checked later

}

stopifnot(i_s %in% c("GIS","AIS", "GLA"))

# GIS need_hist_match = FALSE by default [still true? xxx]


# Fix random seed
set.seed(2024)

# Directory for output files
rdatadir <- "./data-raw/" # RData file containing emulator
outdir <- "./out/" # Everything else
if ( ! file.exists(outdir) ) dir.create(file.path(outdir))

# Directories for input datasets
# (all in the same place, grouped by type)
inputs_preprocess <- paste0(system.file("extdata", package = "emulandice2"), "/")
inputs_ext <- inputs_preprocess
#inputs_preprocess <- "~/PROTECT/emulandice2/inst/extdata/" # Processed: PROTECT data
#inputs_ext <- "~/PROTECT/emulandice2/inst/extdata/" # Pre-existing: GSAT data and AR6 files

#' # Analysis choices
#' ## Dataset, ice source, region [ensemble]

# Choices ------------------------------------------------------------------------
# i_s is name in simulations CSV filename and first column

# IPCC MIPs or PROTECT: no need to keep AR6
# dataset <- "PROTECT"
# stopifnot(dataset %in% c("IPCC_AR6", "PROTECT"))

# xxx Future: test/extend AR6 for glaciers, Antarctica, regions
# if (dataset == "IPCC_AR6") {
#  i_s <- "GrIS" # GrIS, AIS
#  reg <- "ALL"
# }

ensemble_subset <- NA

#if (dataset == "PROTECT") {

# Region set here
# CHOOSE ICE SHEET BASIN (when implemented)
if (i_s %in% c("GIS", "AIS")) {
  reg <- "ALL"
  stopifnot(reg %in% c("ALL")) # will add basins
}

# RGI NUMBER
if (i_s == "GLA") reg <- paste0("RGI", sprintf("%02i", reg_num)) # zero-padded

# Check region name is valid
stopifnot(reg %in% c("ALL", paste0("RGI", sprintf("%02i",1:19))))

#}

# ENSEMBLE DATA

#if (dataset == "PROTECT") {

# Currently two Greenland and glacier ensembles to choose from
if (i_s == "AIS") {
  stopifnot(final_year %in% c(2100, 2150, 2200, 2300))
  ensemble_subset <- "RCM_forced"
}
if (i_s == "GIS") {
  stopifnot(final_year %in% c(2100, 2200, 2300))
  #    ensemble_subset <- "2300"
  #    stopifnot(ensemble_subset %in% c("all", "2300"))
}
if (i_s == "GLA") {

  ensemble_subset <- "PPE" # xxx Now ignored because ensembles are combined - keeping here for now
  stopifnot(ensemble_subset %in% c("forcing", "PPE"))

  if (ensemble_subset == "forcing" && final_year > 2100) {
    warning("ensemble_subset is set to 'forcing' so reducing final_year to 2100")
    final_year <- 2100
    #    if (ensemble_subset == "PPE") final_year <- 2300
  }
  stopifnot(final_year %in% c(2100, 2300))
}
#}

# Long names for outputs
if (i_s == "GIS") ice_name <- "Greenland"  # %in% c("GrIS", "GIS"))
if (i_s == "AIS") ice_name <- "Antarctica"
if (i_s == "GLA") {
  ice_name <- paste("Glaciers:", read.csv(paste0(inputs_ext,"/GLA/regionnames.txt"))[reg_num,1])
}

# Sample size for unif_temps design - used for convenience when adding uncertainty
# (Main effects sample is set in load_design_to_pred.R, and
# AR6 prior sample is equal to number of GSAT projections)
N_prior <- 2000

# Do LOO validation?
do_loo_validation <- FALSE
N_k <- NA # for every N_k-th simulation; NA for full LOO

print("Hello! Welcome to emulandice2: build")

print(paste("Building an emulator for",ice_name,"region",reg,"..."))
if (do_loo_validation) {
  print(paste("LOO with N_k =",N_k,"(could be very slow)"))
}
#' ## Projection times and possible scenarios

# SIMULATION YEARS in dataset i.e. columns in CSV

#if (dataset == "IPCC_AR6") years_sim <- 2015:2100 # no historical
#if (dataset == "PROTECT") {

# Year range of ensemble
# Used to exclude e.g. runs that only go to 2100
# xxx Not ideal: first date is first col in CSV and last date is one we *want*
# xxx is that still true or do we detect first year now?
if (i_s == "GIS") {
  first_year <- 1960
  #    if (ensemble_subset == "all") years_sim <- 1960:2100
  #    if (ensemble_subset == "2300") years_sim <- 1960:2300
}
if (i_s == "AIS") first_year <- 1950 # 2300
if (i_s == "GLA") { # xxx urgh
  first_year <- 1980
  #    if (ensemble_subset == "forcing") years_sim <- 1980:2100
  #    if (ensemble_subset == "PPE") years_sim <- 1980:2300
}
years_sim <- first_year:final_year
#}

# Timeslice frequency to predict (see below: first date is cal_start + nyr)
nyrs = 5 # can be 2 when 2100

# Check reasonably divisible
stopifnot(nyrs %in% c(1, 2, 5, 10)) # Maybe not 2, for FACTS?

# "lhs" is deprecated - see end of code
# but might add updated 2LM prior later
# xxx Future: add SA and make consistent with temp_prior - done?

# Full list of possible emissions scenarios to look for
# (dropped for unif_temps design if not simulated)
scenario_list <- c("SSP119", "SSP126", "SSP245", "SSP370", "SSP585")


#' ## Ice model(s)

#if (dataset == "IPCC_AR6") model_list <- NA # IMAUICE1 for extreme retreat
# Note we cannot predict AR6 AIS as basal melt prior not defined
# xxx still true?

#if (dataset == "PROTECT") {

if (i_s == "GIS") {

  # All models (do not change this)
  model_list_full <- c( "CISM", "IMAUICE", "ElmerIce", "GISM" )

  # Pick models to use: list or CISM only
  if (final_year <= 2100) model_list <- c( "CISM", "IMAUICE") # "GISM" )

  if ( final_year > 2100 ) { # ensemble_subset == "2300") {
    #      if (length(model_list) > 1 ||
    #          (length(model_list) == 1 && model_list != "CISM")) {
    #        warning("Requested final year beyond 2100: resetting model_list to CISM only")
    model_list <- "CISM"
    #  }
  }
  # If ElmerIce: change cal range later to 1992-2014 (if 2 yr timeslices)

  # Require this flag for matching historical + projection retreat values
  # Now rejects only CISM non-matching!! No wonder emulator was borked
  # xxx later just delete this option so always select CISM retreat match
  # xxx better to call this need_retreat_match to avoid confusing with history matching
  need_hist_match <- TRUE

  # xxx check no longer needed/true, because should only :
  if (FALSE) {
    if ( need_hist_match == TRUE &&
         ( length(model_list) > 1 ||
           ( length(model_list) == 1 && model_list != "CISM" )) ) {
      warning("Requested need_hist_match for non-CISM model(s): will reject all runs")
    }
  }

  # Only CISM went beyond 2100 (at all / to any great extent)
  # xxx Should be obsolete, now we change model_list above
  if ( final_year > 2100 && # ensemble_subset == "2300" &&
       ( length(model_list) > 1 ||
         (length(model_list) == 1 && model_list != "CISM") ) ) return()

}

if (i_s == "AIS") {

  model_list_full <- c( "Kori", "PISM" )

  # Pick models to use
  model_list <- model_list_full

  # Obsolete: leave as NA
  melt_param_select <- NA # "PICO"
  # NA for all
  # PICO (Kori and PISM)
  # Burgard, Plume, ISMIP6_nonlocal, ISMIP6_nonlocal_slope (Kori only)
}

if (i_s == "GLA") {

  model_list_full <- c( "GloGEM", "OGGM" )

  # Pick models to use
  model_list <- model_list_full

}

# Check model names are correct
stopifnot( length( setdiff(model_list, model_list_full )) == 0 )

#}

#' ## Set emulator covariance function
# Choose emulator covariance function here so can put in output name for now

# Currently can choose AR6 settings (mostly linear), matern_5_2,
# or pow_exp_19 (pow_exp, alpha = 1.9), 2.0
# XXX Specify by ice sheet region later
if ( i_s == "GIS") emulator_settings <- "pow_exp_20"
if ( i_s == "AIS") emulator_settings <- "pow_exp_10"
if ( i_s == "GLA") emulator_settings <- "pow_exp_20" # for all regions for now
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
# GLACIER CAPS: mm SLE for each region from Farinotti et al.
# as in emulandice v1
max_glaciers <- list()
max_glaciers[["RGI01"]] <- 43.3
max_glaciers[["RGI02"]] <- 2.6
max_glaciers[["RGI03"]] <- 64.8
max_glaciers[["RGI04"]] <- 20.5
max_glaciers[["RGI05"]] <- 33.6
max_glaciers[["RGI06"]] <- 9.1
max_glaciers[["RGI07"]] <- 17.3
max_glaciers[["RGI08"]] <- 0.7
max_glaciers[["RGI09"]] <- 32.0
max_glaciers[["RGI10"]] <- 0.3
max_glaciers[["RGI11"]] <- 0.3
max_glaciers[["RGI12"]] <- 0.2
max_glaciers[["RGI13"]] <- 7.9
max_glaciers[["RGI14"]] <- 6.9
max_glaciers[["RGI15"]] <- 2.1
max_glaciers[["RGI16"]] <- 0.2
max_glaciers[["RGI17"]] <- 12.8
max_glaciers[["RGI18"]] <- 0.2
max_glaciers[["RGI19"]] <- 69.4

# Convert mm to cm
for (rr in names(max_glaciers)) {
  max_glaciers[[rr]] <- max_glaciers[[rr]]/10.0
}

#' ## Baseline and calibration choices

# AR6: 2015 is always start of simulations (no historical)
#
# PROTECT: Earliest Greenland = 1960, Antarctic = 1950, glaciers = 1980
# xxx Do more neatly...!!
# xxx Note cal_start MUST be same as baseline in current code (and makes sense)

# Latest for IMBIE is 2020
# Default 2014 because projections start in 2015
# xxx urgh
# %in% c("GrIS", "GIS")
if (i_s == "GIS") cal_end <- 2015 # 2015 # 2014 for 2100; but 2015 for ElmerIce or 2300 (or 2020?)
if (i_s == "AIS") cal_end <- 2015
if (i_s == "GLA") cal_end <- 2020 # because OGGM fails if too early xxx

# End of IMBIE GIS and Hugonnet; also encroaches onto SSPs; xxx change for AIS?
stopifnot(cal_end <= 2020)

# Start of calibration period
# xxx Tidy up / finalise

#if (dataset == "IPCC_AR6") cal_start = 2015
#if (dataset == "PROTECT") {

# Greenland
# Default start 1992 if nyrs for 2100; but 1995 for 2300
# Earliest Elmer/Ice is 1995; earliest GISM HOM is 2015
# %in% c("GrIS", "GIS"))
if (i_s == "GIS") cal_start = 2000 # 2000 for FACTS or nyrs = 10; sort for calib from 1992

# Antarctica: to hit 2100 with 5 yr timeslices
if (i_s == "AIS") cal_start = 2000 # xxx for FACTS; sort for calib from 1992

# Glaciers: 2000 for most runs, but 2005 for OGGM PPE
if (i_s == "GLA") {
  cal_start = 2000
  #if ( "OGGM" %in% model_list && ensemble_subset == "PPE") cal_start = 2005 # xxx replace when new dataset
}

# Earliest date possible for ice sheets (IMBIE) = 1992
# Glaciers (Hugonnet et al.) = 2000
stopifnot( cal_start >= 1992
           || (i_s == "GLA" && cal_start >= 2000 ) )

#}

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

do_loo_years <- c(2100, 2300) # c(2100, 2200, 2300) # Later add 2015/2020/50
# (Checks these years are emulated later)

if (do_loo_validation) print(paste("LOO years:", paste(do_loo_years, collapse = ",")))

#' ## Emulation input choices

# Emulator settings ------------------------------------------------------------
#_______________
cat("\nEMULATOR INPUTS:\n", file = logfile_build, append = TRUE)

# GSAT timeslices for ice_design
# XXX consider going back earlier?

temps_baseline <- 2015
# Not too many, to avoid linear combinations (esp bad for fixed climate GIS)
# Altered later in code if request shorter projections e.g. to 2100 only
if (i_s == "AIS")  temps_list <- 2300
if (i_s == "GIS") temps_list <- 2100 # 2100 better than 2300 (fixed GSAT after)
if (i_s == "GLA") temps_list <- 2300

# Number of years to average over
# e.g. setting 10 with temps_list = 2300 gives decadal mean 2291-2300
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


# Ice model parameters for ice_design

# AR6: Greenland retreat; Antarctic heat flux; glaciers none
# XXX Add melt0 for GIS
#if (dataset == "IPCC_AR6") ice_param_list <- "melt" # For GrIS and AIS

# PROTECT
# Continuous and categorical (factor) model inputs
if (i_s == "GIS") {

  # Continuous parameters
  # xxx Make init_yrs continuous too?
  # xxx Ignoring retreat_hist for now
  ice_cont_list <- c("retreat", "resolution")

  # xxx p7 2300 hack because only one resolution value and no init_yrs
  # ice_cont_list <- "retreat"

  ice_factor_list_model <- list()

  ice_factor_list_model[["CISM"]] <- c("RCM", "thermodyn", "RCM_init", "init_yrs", "elev_feedback") # "model_variant"

  # xxx p7 hack because different columns then
  # ice_factor_list_model[["CISM"]] <- c("thermodyn", "RCM", "init", "model_variant")

  ice_factor_list_model[["IMAUICE"]] <- c("RCM", "sliding", "SP_climate") #"model_variant")
  ice_factor_list_model[["GISM"]] <- c("RCM") #"model_variant")
  ice_factor_list_model[["ElmerIce"]] <- c("RCM",  "sliding") # "model_variant")


  # ONE MODEL
  if ( length(model_list) == 1) {
    ice_factor_list <- ice_factor_list_model[[ model_list ]]
  } else {

    # ALL POSSIBLE INPUTS
    # will fail if too many
    # ice_factor_list <- c("RCM", "init", "sliding", "thermodyn", "model_variant", "model") # All: p7

    # xxx For now drop SP_climate!! because NA for missing - needs default 'value' instead
    ice_factor_list <- c("RCM", "sliding", "thermodyn", "init", "RCM_init", "init_yrs", "elev_feedback", "model")

  }
}

if (i_s == "AIS") {

  # PISM
  #ice_cont_list <- c("lapse_rate",  "refreeze_frac",
  #                   "PDD_ice", "PDD_snow",
  #                   "heat_flux_PICO")
  #ice_factor_list <- "init_atmos"

  # Kori
  #ice_cont_list <- c("lapse_rate",
  #                   "PDD_ice", "PDD_snow", "PDD_sd", "refreeze",
  #                   "heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard",
  #                   "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")

  # Both: GCM-forced
  ice_cont_list <- c("lapse_rate",  "refreeze_frac",
                     "PDD_ice", "PDD_snow", "PDD_sd", "refreeze",
                     "heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard",
                     "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")

  # GCM forced
  ice_cont_list <- c(
    "resolution", "PDD_sd",
    "refreeze_frac",  # PDD (GCM only)
    "heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard",
    "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")
  #"overturning_PICO") # PICO only
  # "lapse_rate", "refreeze" "PDD_ice", "PDD_snow", "sliding_exponent", # need to impute RCM
  # "heat_flux_ISMIP6_local", # xxx not used?
  #"tillwater_decay_rate", "eff_fraction_overburden_pressure")

  ice_factor_list <- c("Phase", "init_atmos", "init_ocean", "melt_param", "model",
                       "forcing_type") #"sliding_law" )

  # Kori only
  if (ensemble_subset == "RCM_forced") {
    ice_cont_list <- c("heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard",
                       "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope")
    ice_factor_list <- c("melt_param")
  }


  # "init_method",

  # Add factor: model
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
  ice_cont_list <- unique( c(ice_cont_list_model[["GloGEM"]], ice_cont_list_model[["OGGM"]]))

  # Ensemble is for any setup differences, e.g.:
  # For OGGM, forcing uses reanalysis 2000-2020 and parameter uses GM
  # For GloGEM, forcing parameters are regional means over glaciers but parameter ensemble has same value everywhere
  ice_factor_list <- c("ensemble","model")

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

# IPCC AR6 emulator, emulandice v1
if ( emulator_settings == "IPCC_AR6") {

  # Most were power exponential
  kernel <- "pow_exp"

  if ( reg == "RGI12") kernel <- "matern_3_2"
  if ( reg %in% c("RGI17", "RGI19") ) kernel <- "matern_5_2"
  lower_bound = FALSE # xxx FALSE was just to make it run so maybe ditch?

  # Most emulators were power exponential
  if (kernel == "pow_exp") {

    # 0.1: covariance has small effect; approaches linear regression
    # 1.9: close to squared exponential
    alpha = 0.1
    if (reg %in% paste0("RGI",c("01","06","08","09","10","11"))) alpha = 1.0
    if (reg %in% paste0("RGI",c("02","03","14"))) alpha = 1.9 # default
  } else alpha = NA
}

# Default in RG is to bound;
# Set to FALSE in AR6 because warning when searching
# "NA/Inf replaced by maximum positive value"
# and to check for inert inputs

if (emulator_settings == "matern_5_2") {
  kernel <- "matern_5_2"
  lower_bound <- TRUE # default = TRUE
  alpha = NA
}

if (emulator_settings == "matern_3_2") {
  kernel <- "matern_3_2"
  lower_bound <- TRUE # default = TRUE
  alpha = NA
}
if (emulator_settings == "pow_exp_01") {
  kernel <- "pow_exp"
  lower_bound <- TRUE # default = TRUE
  alpha = 0.1
}
if (emulator_settings == "pow_exp_10") {
  kernel <- "pow_exp"
  lower_bound <- TRUE # default = TRUE
  alpha = 1.0
}
if (emulator_settings == "pow_exp_19") {
  kernel <- "pow_exp"
  lower_bound <- TRUE # default = TRUE
  alpha = 1.9 # default for pow_exp
}
if (emulator_settings == "pow_exp_20") {
  kernel <- "pow_exp"
  lower_bound <- TRUE # default = TRUE
  alpha = 2.0
}

stopifnot(kernel %in% c("pow_exp", "matern_5_2", "matern_3_2"))


#' ## Plot choices
# Plot settings ------------------------------------------------------------

# Plot all or just subset of figures
plot_level <- 2 # 0 for none, 1 for main, 2 for exhaustive

# Quantiles to output [to text?]
q_list <- c( 0.50, 0.05, 0.95, 0.17, 0.83, 0.25, 0.75 )

# Sub-sample to plot; exclude any dates not predicted by emulator
yy_plot <- c(cal_end,"2100", "2300")
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

if (i_s == "GLA") {

  # 0.1 and 0.5 for others than 3?
  ylim <- c(-2, 1.1*max_glaciers[[reg]])
  ylim_max <- c(-8, 1.5*max_glaciers[[reg]])

  # Adjust lower end for dinky glacier regions
  if (max_glaciers[[reg]] < 1.0) {
    ylim[1] <- -0.005
    ylim_max[1] <- -0.01
  }
  #  if (reg %in% c("RGI03")) ylim <- c(-2,7)  # max or 2100?
  #  if (reg %in% c("RGI12", "RGI18")) ylim <- c(-0.005,0.03) # max or 2100?
}
if (i_s == "AIS") {
  # xxx do list by timeslice?
  ylim <- c(-50,150)  # 2100
  ylim_max <- c(-400, 1200) # 2300
}
if (i_s == "GIS") {  # %in% c("GrIS", "GIS")) {
  # xxx do list by timeslice?
  ylim <- c(-10,40)  # 2100
  ylim_max <- c(-200, 500) # 2300
}

# But put max limit down if doing short timescale tests
if (max(years_sim) <= 2150) ylim_max <- ylim

# IPCC AR6 colours
AR6_rgb <- list()
AR6_rgb[["SSP119"]] <- rgb(30, 150, 132, maxColorValue = 255) # SSP1-19
AR6_rgb[["SSP126"]] <- rgb(29, 51, 84, maxColorValue = 255) # SSP1-26
AR6_rgb[["SSP245"]] <- rgb(234, 221, 61, maxColorValue = 255) # SSP2-45
AR6_rgb[["SSP370"]] <- rgb(242, 17, 17, maxColorValue = 255) # SSP3-70
AR6_rgb[["SSP460"]] <- rgb(232, 136, 49, maxColorValue = 255) # SSP4-60
AR6_rgb[["SSP585"]] <- rgb(132, 11, 34, maxColorValue = 255) # SSP5-85

# 60% transparency
AR6_rgb_med <- list()
AR6_rgb_med[["SSP119"]] <- rgb(30, 150, 132, maxColorValue = 255, alpha = 153) # SSP1-19
AR6_rgb_med[["SSP126"]] <- rgb(29, 51, 84, maxColorValue = 255, alpha = 153) # SSP1-26
AR6_rgb_med[["SSP245"]] <- rgb(234, 221, 61, maxColorValue = 255, alpha = 153) # SSP2-45
AR6_rgb_med[["SSP370"]] <- rgb(242, 17, 17, maxColorValue = 255, alpha = 153) # SSP3-70
AR6_rgb_med[["SSP460"]] <- rgb(232, 136, 49, maxColorValue = 255, alpha = 153) # SSP4-60
AR6_rgb_med[["SSP585"]] <- rgb(132, 11, 34, maxColorValue = 255, alpha = 153) # SSP5-85

# 10% transparency; 20% = 51
AR6_rgb_light <- list()
AR6_rgb_light[["SSP119"]] <- rgb(30, 150, 132, maxColorValue = 255, alpha = 25) # SSP1-19
AR6_rgb_light[["SSP126"]] <- rgb(29, 51, 84, maxColorValue = 255, alpha = 25) # SSP1-26
AR6_rgb_light[["SSP245"]] <- rgb(234, 221, 61, maxColorValue = 255, alpha = 25) # SSP2-45
AR6_rgb_light[["SSP370"]] <- rgb(242, 17, 17, maxColorValue = 255, alpha = 25) # SSP3-70
AR6_rgb_light[["SSP460"]] <- rgb(232, 136, 49, maxColorValue = 255, alpha = 25) # SSP4-60
AR6_rgb_light[["SSP585"]] <- rgb(132, 11, 34, maxColorValue = 255, alpha = 25) # SSP5-85

# PLOT RANGES

# SL simulations are up with max_glaciers

# Observations
#if (dataset == "IPCC_AR6") ylim_obs <- c(-1,1.5)
#if (dataset == "PROTECT") {
if (i_s == "GIS") ylim_obs <- c(-1.5,2)
if (i_s == "AIS") ylim_obs <- c(-10,10)

# Hugonnet for glaciers, not IMBIE!
# and do list or similar for regions
if (i_s == "GLA") {
  ylim_obs <- c(-0.5,2) # 1 for RGI05 LOO?
  if (reg %in% c("RGI12", "RGI18")) ylim_obs <- c(-0.01,0.03)
}
#}




#' # Load and process data

#' ## Load climate and ice simulations
# Get sims ---------------------------------------------------------------------

# GET CLIMATE AND LAND ICE SIMULATIONS
# Returns CSV file data
climate_data <- emulandice2::load_sims(variable = "climate") # dataset
ice_data <- emulandice2::load_sims(variable = "ice", source = i_s) # dataset

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

# Select ice source, region, model(s) and any other exclusions
ice_data <- emulandice2::select_sims()

# Calculate SLE change w.r.t. cal_start year, and tidy units
ice_data <- emulandice2::calculate_sle_anom()

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
} #  expression(paste('Global mean temperature change 2015-2100 (',degree~C,')')))


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

    # Could use this structure instead of for loop?
    # dummy <- matrix(NA, nrow = nrow(ice_data), ncol = length(ff_vals))

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

#' ## Load observations
# Get obs -------------------------------------------------------------------

#if ( ! exists("obs_data") )
obs_data <- emulandice2::load_obs()

#' # Set model discrepancy
# Model error -----------------------------------------------------------------------

# Model discrepancy
# xxx Using multiple of obs error for now
# Glacier error very small so use large scaling factor
if (i_s == "GLA") { scale_mod_err = 20
} else scale_mod_err = 3
stopifnot( scale_mod_err > 1 )
model_err <- scale_mod_err * obs_data[,"SLE_sd"]

cat(paste("\nModel error: using",scale_mod_err,"x obs error", "\n"),
    file = logfile_build, append = TRUE)

# Calculate combined discrepancy
total_err <- sqrt(obs_data[,"SLE_sd"]^2 + model_err^2)

#' # Plot simulations
# Plot sims -----------------------------------------------------------------------

cat("\nPlot simulator projections:\n", file = logfile_build, append = TRUE)

# Plot simulations (some with observations)
if (plot_level > 0) {
  pdf( file = paste0( outdir, out_name, "_SIMS.pdf"),
       width = 9, height = 5)
  emulandice2::plot_designs("sims", plot_level)
  emulandice2::plot_timeseries("sims", plot_level)
  emulandice2::plot_scatter("sims", "none", plot_level)
  emulandice2::plot_distributions("sims", plot_level)
  dev.off()
}

#' # Build emulator
# Build emulator -----------------------------------------------------------------------

if (TRUE) {

  make_emu <- function(designX, responseF, r = NULL, thresh = 0.99) {

    # PUT MAKE_EMU() HERE BECAUSE NOT WORKING WHEN IN FUNCTION
    # ARGUMENTS:
    #    designX <- ice_design_scaled
    #    responseF <- as.matrix( ice_data[ , paste0("y", years_em) ] )
    #    r <- NULL
    #    thresh <- 0.99

    cat("_____________________________________\n", file = logfile_build, append = TRUE)
    cat("make_emu: building emulator...\n", file = logfile_build, append = TRUE)

    stopifnot(is.matrix(designX))
    m <- nrow(designX)
    d <- ncol(designX)
    stopifnot(is.matrix(responseF), nrow(responseF) == m)
    n <- ncol(responseF)
    if (!is.null(r))
      stopifnot(r == round(r), 0 < r, r <= n)
    stopifnot(length(thresh) == 1, 0 < thresh, thresh < 1)

    ## SVD

    cc <- colMeans(responseF)
    # Use sweep to centre data (subtract column means from columns of responseF)
    # then do SVD i.e. a PCA
    decomp <- svd(sweep(responseF, 2L, cc, "-"))
    dd2 <- decomp$d^2
    scree <- cumsum(dd2) / sum(dd2)
    if (is.null(r))
      r <- which.max(scree >= thresh) # first exceedance
    U <- decomp$u[, 1L:r, drop=FALSE]
    Vt <- (decomp$d * t(decomp$v))[1L:r, , drop=FALSE]

    ## write a message

    cat(sprintf("make_emu: r = %i, scree = %.1f%%\n", r, 100 * scree[r]), file = logfile_build, append = TRUE)

    ## build emulators, hide rgasp output

    sink(file = paste0(outdir,out_name,"_rgasp.log"))

    trendX <- designX

    # Drop factors (dummy variable columns) from trend
    if (include_factors) {

      cat("Dropping factors from trend:\n", file = logfile_build, append = TRUE)
      cat(paste(c(ice_dummy_list, "\n"), collapse = " "), file = logfile_build, append = TRUE)

      trendX <- trendX[ , input_cont_list]

      cat("\nKeeping:\n", file = logfile_build, append = TRUE)
      cat(paste(c(colnames(trendX), "\n"), collapse = " "), file = logfile_build, append = TRUE)
    }

    EMU <- lapply(1L:r, function(j) {

      # Default values for RobustGaSP
      initial_values <- NA
      max_eval <- max(30,20+5*dim(designX)[2])

      # xxx Quick hack for AIS two model: use initial values from main build,
      # only try one set of initial values, and
      # max_eval
      # Last value is nugget
      if (FALSE) { # Fails: Error in construct_rgasp(model@beta_hat, model@nugget, model@R0, model@X,  :
        # Expecting a single value: [extent=0]
        if (i_s == "AIS" && length(model_list) == 2) {
          max_eval <- 30
          if (j == 1) {
            # inverse ln of 1st iteration values in rgasp.log (just ln for last: nugget)
            iv <- c(-0.348583597,-5.982693695,-2.879534918,-4.512419937,-4.100360431,-5.980538323,-7.745731975,-3.817967239,-3.971741429,-3.447429356,-4.819408604,-4.751369624,-7.657114537,-2.741552028,-1.764697748,-1.334600197,-3.466662661,-2.175281468,-2.500552775,-2.775645919,-1.764697748,-6.999510216)
          }
          if (j == 2) {
            # 1st iteration values
            #iv <- c(0.749065893,-5.528651007,-2.208472526,-5.046248868,-4.688992818,-6.318759804,-13.21427999,-3.498131091,-2.737487313,-3.078379433,-3.479271075,-3.19315137,-11.56413081,-2.324335822,-1.264418775,-0.108093577,-2.224375953,-0.976333537,-3.005910796,-2.454771273,-1.264418775,-22.83327546)
            # 2nd iteration values
            iv <- c(-0.334440889,-5.578491701,-2.382315444,-5.018006063,-4.733133229,-6.633783194,-13.21427999,-3.731799087,-3.701152827,-3.514179281,-3.754871528,-3.559593039,-11.56413081,-2.466600952,-1.34296532,-0.701867547,-2.294487597,-1.277426312,-3.465961502,-3.231161644,-1.34296532,-5.67155907)
          }
          # Add a bit of noise for 2nd iteration
          initial_values <- rbind(iv, iv + runif(length(iv), 0, 1e-4))
        }
      } # FALSE
      #warning(paste0("Number of NAs in design:", length(designX[ is.na(designX) ])))
      #U_j = U[, j]
      #warning(paste0("Number of NAs in response:", length(U_j[ is.na(U_j) ])))
      #warning(paste0("Number of NAs in trend:", length(trendX[ is.na(trendX) ])))

      RobustGaSP::rgasp(design = designX, response = U[, j], trend = cbind(1, trendX),
                        nugget.est = TRUE , lower_bound = lower_bound,
                        kernel_type = kernel, alpha = rep(alpha, dim(as.matrix(designX))[2]))#,
      #                      max_eval = max_eval, initial_values = initial_values)
    })

    sink()

    ## predict method returns a list

    pred_EMU <- function(designXout) {

      trendXout <- designXout

      # Drop any factors from trend
      if (include_factors) {
        tt <- which( input_cont_list %in% colnames(ice_design), arr.ind = TRUE )
        trendXout <- trendXout[ , tt, drop = FALSE]
      }

      lapply(EMU, function(emu) {
        RobustGaSP::predict(emu, testing_input = designXout,
                            testing_trend = cbind(1, trendXout))[c("mean", "sd")]
      })
    }

    ## return a function

    robj <- function(designXout, type = c("mean", "sd", "var", "all")) {

      type <- match.arg(type)
      if (!is.matrix(designXout) && length(designXout) == d) {
        dim(designXout) <- c(1L, d)
      } else {
        stopifnot(is.matrix(designXout), ncol(designXout) == d)
      }
      m_out <- nrow(designXout)
      pplist <- pred_EMU(designXout) # r-list

      ## compute the mean

      mu <- sapply(pplist, "[[", "mean") # m_out x r
      dim(mu) <- c(m_out, r)
      mx <- sweep(mu %*% Vt, 2L, cc, "+") # m_out x n
      if (type == "mean")
        return(list(mean = mx))

      ## compute the sd

      sdu <- sapply(pplist, "[[", "sd") # m_out x r
      dim(sdu) <- c(m_out, r)
      #if (type == "sd") {
      sdx <- lapply(1L:m_out, function(i) {
        sqrt(colSums((sdu[i, ] * Vt)^2)) # n vector
      })
      sdx <- do.call("rbind", sdx) #  m_out x n
      if (type == "sd") return(list(mean = mx, sd = sdx))
      #}

      ## compute the variance; TLE changed to return sd too


      Sx <- lapply(1L:m_out, function(i) {
        as.vector(crossprod(sdu[i, ] * Vt)) # n*n vector
      })
      Sx <- do.call("cbind", Sx) # n*n x m_out
      dim(Sx) <- c(n, n, m_out)
      Sx <- aperm(Sx, c(3, 1, 2)) # I wonder who wrote aperm :)
      return(list(mean = mx, sd = sdx, var = Sx))
    }

    ## class and return

    cat("make_emu: end of emulator build\n",file = logfile_build, append = TRUE)
    cat("_____________________________________\n",file = logfile_build, append = TRUE)

    #    emu_mv <- structure(robj, class = "emu")
    structure(robj, class = "emu")

  }
}

# END MAKE_EMU()

#_______________________________________________________________________________

# Settings are hard-coded at the top
# Writes emu obj from .RData workspace file later for running in FACTS

# Could use this for now if exists and unchanged: if ( ! exists("emu_mv") )

# FUNCTION NOT WORKING SINCE A PACKAGE - SEE CODE ABOVE INSTEAD
#if (FALSE) {

# Build emulator for timeslices every nyrs
# Note this call is repeated in do_LOO.R
if ( ! exists("emu_mv") ) {
  #show(system.time(
  emu_mv <- make_emu( ice_design_scaled,
                      as.matrix( ice_data[ , paste0("y", years_em) ] ))
  #))
} else {
  message("Not rebuilding emulator! Use only for testing code or slow build emulators")
}

#} # don't build emu_mv from function make_emu()


#' # Predict for SA designs
# Design: ME -----------------------------------------------------------------------

#' ## Main effects
# Main effects (i.e. one-at-a-time design for sensitivity analysis)
design_sa <- emulandice2::load_design_to_pred("main_effects")

# Predict
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

# Plot: SA ----------------------------------------------------------------------

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

# Add uncertainty ----------------------------------------------------------------------
projections <- list()

# Want to see unif_temps final projections (samples with uncertainty) for validation
for (scen in scenario_list) {
  projections[[scen]] <- emulandice2::emulator_uncertainty(myem[[scen]])
}

# Plot sensitivity analysis
if (plot_level > 0) {
  pdf( file = paste0( outdir, out_name, "_SA.pdf"),
       width = 9, height = 5)
  emulandice2::plot_scatter("prior", "main_effects",plot_level)
  emulandice2::plot_scatter("prior", "unif_temps",plot_level)
  emulandice2::plot_scatter("posterior", "unif_temps",plot_level) # overkill?
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

    # Run LOO prediction (in do_loo.R)
    loo_mean[[yind]] <- loo_valid_all$mean[ , yind]
    loo_sd[[yind]] <- loo_valid_all$sd[ , yind]

    # N_k selection of runs
    N_k_index <- !is.na(loo_mean[[yind]])
    N_k_subset <- length( loo_mean[[yind]][ N_k_index ]  )

    # Which ones were within predicted intervals and which ones missed?
    wrong[[ yind ]] <- ice_data[ , yind] > ( loo_mean[[yind]] + 2*loo_sd[[yind]] ) |
      ice_data[ , yind] < (loo_mean[[yind]]  - 2*loo_sd[[yind]])

    # Fraction that missed
    # frac_right <- 1 - (length(ice_data[ wrong[[ yind ]][N_k_index] , yind]) / N_k_subset)
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






