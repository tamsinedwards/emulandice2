#!/bin/bash
#
# Run emulandice2 full analysis for AIS
#
# build emulator: Rscript ...
# predict: ./emulandice_steer.sh ...
#
# ./run_AIS.sh -y final_year [-c config] [-d build_date] [-t build | predict]
#
# Must set:
# -y final_year: 2100, 2150, 2200 or 2300
#
# Options:
# -c config: YML file in ./inst -> if not set uses default name in emulator_build.R
#            Note arg sets one YML file for all regions
# -d build_date: YYMMDD -> if not set, uses today's date to write and/or use .RData file
#    i.e. only specify build_date if running predict on older build files
# -t type: build/predict -> if not set, runs both build and predict stages
#
#______________________________________________________

# SSP list if predicting
# ssp_list="ssp119 ssp126" "ssp245" "ssp370" "ssp534-over" "ssp585"
ssp_list="ssp126 ssp370 ssp585"

# IPCC AR6: FaIR 2LM
gsat_file=twolayer_SSPs.h5

# Specify emulandice2 and results directories
# Config file must be in package directory ./inst
# Predict call assumes emulator build .RData file is in package directory ./data-raw
# and looks for climate netcdf/hcdf file in gsat_dir
emulandice_dir=/Users/tamsinedwards/PROTECT/emulandice2
results_dir=/Users/tamsinedwards/PROTECT/RESULTS
gsat_dir=/Users/tamsinedwards/PROTECT/gsat

#______________________________________________________

echo
echo "Running emulandice2 AIS..."
echo

usage_string="Usage: ./run_AIS.sh -y final_year [-c config] [-d build_date] [-t build | predict]"

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

if [ "$final_year" != 2100 -a "$final_year" != 2150 -a "$final_year" != 2200 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100, 2150, 2200 or 2300"
     exit 1
fi

# Today's date
now=$(date +'%y%m%d')

# Build date defaults to today if not given
build_date="${build_date:-$now}"

# Seed for prediction
seed=2024

run_type="${run_type:-"build and predict"}"
echo "Run type:" $run_type

# Dated name for directory
outdir="$results_dir"/"$now"_AIS_ALL_"$final_year" # put all regions in one directory
echo "Output dir:" $outdir

########################################
# REGION LOOP
########################################

# Run total and/or 3 regions
for region in "WAIS" "EAIS" "PEN" # "ALL"
do

  echo
  echo "region: $region"

  ########################################
  # BUILD
  ########################################

  if [[ "$run_type" != "predict" ]]
  then

    echo
    echo run AIS: build
    echo

    # Use default file in package if not specified
    if [ "$config" != "" ]; then
      echo "Build configuration file:" "./inst/"$config
    fi
    if [ "$config" = "" ]; then
      echo "Build configuration file not specified: using default file in" "./inst/"
    fi

    # RUN EMULANDICE2 EMULATOR BUILD
    Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" AIS $region $final_year $config

  fi

  ########################################
  # PREDICT
  ########################################

  if [[ "$run_type" != "build" ]]
  then

    echo
    echo run AIS: predict
    echo

    echo "FaIR GSAT file:" $gsat_file
    echo "SSPs:" $ssp_list

    build_file="AIS_"$region"_"$final_year"_"$build_date"_EMULATOR.RData"
    echo "Build file:" ./data-raw/"$build_file"
    echo

    for ssp in $ssp_list
     do

      echo
      echo "Scenario:" $ssp

      # RUN EMULANDICE2 PREDICT SSP
      ./emulandice_steer.sh AIS $region ./data-raw/"$build_file" "$gsat_dir"/"$gsat_file" $ssp ./out/AIS_"$region"_"$final_year"_"$ssp"/ $seed AIS_"$region"_"$final_year"_"$ssp"

   done
  fi

done # regions

# Won't move if predictions exist already
mkdir $outdir
mv "$emulandice_dir"/out/AIS* "$emulandice_dir"/data-raw/AIS*.RData $outdir
