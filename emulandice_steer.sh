#!/bin/bash
# Run emulandice2 module

ice_source=$1 # Ice source: GIS, AIS or GLA
region=$2 # Ice source region: ALL for GIS/AIS and RGI01-RGI19 for GLA
emu_file=$3 # full path to build file: must match two settings above
climate_data_file=$4 # e.g. emulandice.ssp585.temperature.fair.temperature_climate.nc
scenario=$5 # e.g. ssp585 [could extract from filename instead?]
outdir=$6 # name of directory to put outputs - will attempt to create if it doesn't exist
seed=$7
pipeline_id=$8

if [ $# != 8 ] && [ $# != 0 ];
then
      echo "Wrong number of arguments given"
      echo "Required: ice_source region emu_file climate_data_file scenario outdir seed pipeline_id"
      echo "Or if no arguments are set, will run in test mode"
     exit 1
fi

Rscript --vanilla -e "library(emulandice2)" -e "source('main.R')" $ice_source $region $emu_file $climate_data_file $scenario $outdir $seed $pipeline_id

# Arguments to add ____________
# Read from emulator RData file:
# baseyear # 2000
# pyear_start # 2005
# pyear_end # 2100 or 2300
# pyear_step # 5

# Other:
# nsamps # could add if want to deviate from number of FaIR samples or hard check on this

# Build options for emu_name used in PROTECT Fall 2023 meeting results:
# AIS ALL Kori_PISM_pow_exp_10
# GIS ALL CISM_IMAUICE_pow_exp_20
# GIS ALL CISM_pow_exp_20
# GLA RGI03 GloGEM_OGGM_pow_exp_20

#=====================================================================

# FACTS emulandice v1:
# emulandice_dataset=$1
# nsamps=$2
# ice_sources=$3
# echo Rscript -e "source('packrat/init.R')" -e "library(emulandice)" -e "emulandice::main('decades',dataset='$emulandice_dataset',N_FACTS=$nsamps,ice_sources=c('$ice_sources'))"
# Rscript -e "source('packrat/init.R')" -e "library(emulandice)" -e "emulandice::main('decades',dataset='$emulandice_dataset',N_FACTS=$nsamps,ice_sources=c('$ice_sources'))"

# where
# main <- function(expt = "default",
#                 ice_sources = c("GrIS", "AIS", "Glaciers"),
#                 years = 2100,
#                 dataset = "main",
#                 N_temp = 1000L,
#                 N_FACTS =  2237L,
#                 outdir = "results",
#                 temp_prior = "FAIR",
#                 fair_ssps = NA,
#                 mean_temp = FALSE,
#                 gamma0_prior = "joint",
#                 mean_melt = FALSE,
#                 collapse_prior = "both",
#                 risk_averse = FALSE,
#                 min_res_is = c(NA, NA),
#                 select_ism = "all",
#                 select_gcm = "all",
#                 history_match = FALSE,
#                 impute_high = FALSE,
#                 exclude_open = FALSE,
#                 do_model_comp = FALSE,
#                 do_covar_fn = NA,
#                 do_covar_alpha = NA,
#                 packagename = "emulandice") {
