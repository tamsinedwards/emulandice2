#!/bin/bash
# Run emulandice2

ice_source=$1
region=$2
emu_name=$3 # model_list_emulator_settings: e.g. "CISM_pow_exp_20", "CISM_IMAUICE_GISM_pow_exp_20"

# Options used in PROTECT Fall 2023 meeting results:
# AIS ALL Kori_PISM_pow_exp_10
# GIS ALL CISM_IMAUICE_pow_exp_20
# GIS ALL CISM_pow_exp_20
# GLA RGI03 GloGEM_OGGM_pow_exp_20

Rscript --vanilla -e "library(emulandice2)" -e "source('main.R')" $ice_source $region $emu_name



# expt = decades --> in design
# dataset = "main",
# N_FACTS =  2237L,
# ice_sources = c("GrIS", "AIS", "Glaciers"),

# Build emulandice R package - to create tarball emulandice_2.0.0.tar.gz
# R --vanilla CMD build emulandice

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
