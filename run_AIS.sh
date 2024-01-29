#!/bin/bash
# Run AIS analysis
# ./run_AIS.sh final_year
# where final_year is 2100, 2150, 2200 or 2300

# Check plot_level = 1 in main.R

# 2300 or 2100
final_year=$1

if [ "$final_year" != 2100 -a "$final_year" != 2150 -a "$final_year" != 2200 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100 or 2300"
     exit 1
fi


# Moved to today's directory
now=$(date +'%y%m%d')
outdir=/Users/tamsinedwards/PROTECT/RESULTS/"$now"_AIS_ALL_"$final_year"

echo Build
Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" AIS 0 $final_year

echo
echo "Predict"

  for ssp in "ssp126" "ssp245" "ssp585"
  do

  echo "Scenario:" $ssp

  if [ "$final_year" == 2300 ]
  then
  ./emulandice_steer.sh AIS ALL ./data-raw/AIS_ALL_Kori_PISM_pow_exp_10_EMULATOR.RData ./inst/extdata/GSAT/bamber19."$ssp".temperature.fair.temperature_climate.nc $ssp ./out/AIS_ALL_"$ssp"_"$final_year"/ 2024 AIS_ALL_"$ssp"_"$final_year"
  fi
  if [ "$final_year" == 2100 ]
  then
  ./emulandice_steer.sh AIS ALL ./data-raw/AIS_ALL_Kori_PISM_CISM_ElmerIce_pow_exp_10_EMULATOR.RData ./inst/extdata/GSAT/bamber19."$ssp".temperature.fair.temperature_climate.nc $ssp ./out/AIS_ALL_"$ssp"_"$final_year"/ 2024 AIS_ALL_"$ssp"_"$final_year"
  fi

  done

# Won't copy if predictions exist
mkdir $outdir
mv /Users/tamsinedwards/PROTECT/emulandice2/out/AIS* ~/PROTECT/emulandice2/data-raw/AIS* $outdir
