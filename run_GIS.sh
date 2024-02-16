#!/bin/bash
#
# Run GIS analysis
# ./run_GIS.sh final_year
# where final_year argument can be 2100 or 2300
#
#______________________________________________________

# Specify emulandice2 and results directories
emulandice_dir=/Users/tamsinedwards/PROTECT/emulandice2
results_dir=/Users/tamsinedwards/PROTECT/RESULTS
gsat_dir=/Users/tamsinedwards/PROTECT/gsat

# Assumes build file is in package directory ./data-raw
# and climate file is in package directory ./inst/extdata/GSAT

#______________________________________________________

# Final year is command line argument
final_year=$1

if [ "$final_year" != 2100 -a "$final_year" != 2200 -a "$final_year" != 2250 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100, 2200, 2250 or 2300"
     exit 1
fi

# Moved to today's directory
now=$(date +'%y%m%d')
outdir="$results_dir"/"$now"_GIS_ALL_"$final_year"

echo run GIS: build
Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS 0 $final_year

echo
echo run GIS: predict

if [ "$final_year" -gt 2100 ]
then
  build_file=GIS_ALL_CISM_pow_exp_01_EMULATOR.RData
fi
if [ "$final_year" == 2100 ]
then
  build_file=GIS_ALL_CISM_IMAUICE_ElmerIce_GISM_pow_exp_01_EMULATOR.RData
fi

echo $build_file
echo

for ssp in "ssp119" "ssp126" "ssp245" "ssp370" "ssp585"
  do

  echo "Scenario:" $ssp

  # IPCC AR6: FaIR 2LM
  gsat_file=twolayer_SSPs.h5

  # Victor test files: FaIR 3LM
  # gsat_file="$ssp".temperature.fair.temperature_climate.nc

  echo "GSAT file:" $gsat_file

  ./emulandice_steer.sh GIS ALL ./data-raw/"$build_file" "$gsat_dir"/"$gsat_file" $ssp ./out/GIS_ALL_"$ssp"_"$final_year"/ 2024 GIS_ALL_"$ssp"_"$final_year"

  done

# Won't move if predictions exist already
mkdir $outdir
mv "$emulandice_dir"/out/GIS* "$emulandice_dir"/data-raw/GIS* $outdir

