#!/bin/bash
#
# Run emulandice2 full analysis for Greenland
#
# build emulator: Rscript ...
# predict: ./emulandice_steer.sh ...
#
# ./run_GIS.sh -y final_year [-c config] [-d build_date] [-t build | predict]
#
# Must set:
# -y final_year: 2100, 2150, 2200, 2250 or 2300
#
# Options:
# -c config: YML file in ./inst -> if not set uses default name in emulator_build.R
# -d build_date: YYMMDD -> if not set, uses today's date to write and/or use .RData file
#    i.e. only specify build_date if running predict on older build files
# -t type: build/predict -> if not set, runs both build and predict stages
#
#______________________________________________________

# SSP list if predicting
# ssp_list="ssp119 ssp126" "ssp245" "ssp370" "ssp534-over" "ssp585"
ssp_list="ssp126 ssp370 ssp585"

# Specify emulandice2 and results directories
# Config file must be in package directory ./inst
# Predict call assumes emulator build .RData file is in package directory ./data-raw
# and looks for climate netcdf/hcdf file in gsat_dir
emulandice_dir=/Users/tamsinedwards/PROTECT/emulandice2
results_dir=/Users/tamsinedwards/PROTECT/RESULTS
gsat_dir=/Users/tamsinedwards/PROTECT/gsat

#______________________________________________________

echo
echo "Running emulandice2 GIS..."
echo

usage_string="Usage: -y final_year [-c config] [-d build_date] [-t build | predict]"

while getopts "y:c:d:t:" opt; do
    case $opt in
        y) final_year=$OPTARG; echo "Year: $final_year" ;;
        c) config=$OPTARG ;;
        d) build_date=$OPTARG ;;
        t) run_type=$OPTARG ;;
    esac
done

if [ $# -eq 0 ]; then
    echo "No arguments provided. Must provide at least the final year."
    echo $usage_string
    exit 1
fi

if [ $# -eq 1 -o $# -eq 3 -o $# -eq 5 -o $# -eq 7 ]; then
    echo "Wrong syntax: expected even number of arguments."
    echo $usage_string
    exit 1
fi

if [ $# -gt 8 ]; then
    echo "Too many arguments."
    echo $usage_string
    exit 1
fi

if [ "$final_year" != 2100 -a "$final_year" != 2150 -a "$final_year" != 2200 -a "$final_year" != 2250 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100, 2150, 2200, 2250 or 2300"
     exit 1
fi

# Today's date
now=$(date +'%y%m%d')

# Build date defaults to today if not given
build_date="${2:-$now}"

# Seed for prediction
seed=2024

run_type="${run_type:-"build and predict"}"
echo "Run type:" $run_type

# Output/source build file name
build_file=GIS_ALL_"$final_year"_"$build_date"_EMULATOR.RData
echo "Build file:" ./data-raw/"$build_file"

# Dated name for directory
outdir="$results_dir"/"$now"_GIS_ALL_"$final_year"
echo "Output dir:" $outdir

########################################
# BUILD
########################################

if [[ "$run_type" != "predict" ]]
then

  echo
  echo run GIS: build
  echo

  # Blank if not specified
  #config_file="${config:-" "}"
  if [ $config != "" ]; then
    echo "Build configuration file:" "./inst/"$config
  fi
  if [ $config = "" ]; then
    echo "Build configuration file not specified: using default file in" "./inst/"
  fi

  # RUN EMULANDICE2 EMULATOR BUILD
  Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS ALL $final_year $config

fi

########################################
# PREDICT
########################################

if [[ $run_type != "build" ]]
then

echo
echo run GIS: predict
echo

# IPCC AR6: FaIR 2LM
gsat_file=twolayer_SSPs.h5
echo "FaIR GSAT file:" $gsat_file

echo "SSPs:" $ssp_list

for ssp in $ssp_list
  do

  echo
  echo "Scenario:" $ssp

  # RUN EMULANDICE2 PREDICT SSP
  ./emulandice_steer.sh GIS ALL ./data-raw/"$build_file" "$gsat_dir"/"$gsat_file" $ssp ./out/GIS_ALL_"$final_year"_"$ssp"/ $seed GIS_ALL_"$final_year"_"$ssp"

  done

fi # if predicting

echo
echo "Creating" $outdir "if it does not exist"

# Won't move if predictions exist already
mkdir $outdir
mv "$emulandice_dir"/out/GIS* "$emulandice_dir"/data-raw/GIS*RData $outdir

echo
