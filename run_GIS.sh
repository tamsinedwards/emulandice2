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

#______________________________________________________

# Final year is command line argument
final_year=$1

if [ "$final_year" != 2100 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100 or 2300"
     exit 1
fi

# Moved to today's directory
now=$(date +'%y%m%d')
outdir="$results_dir"/"$now"_GIS_ALL_"$final_year"

echo run_GIS: build
Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS 0 $final_year

echo
echo run_GIS: predict

for ssp in "ssp126" "ssp245" "ssp585"
  do

  echo "Scenario:" $ssp
  if [ "$final_year" == 2300 ]
  then
    ./emulandice_steer.sh GIS ALL ./data-raw/GIS_ALL_CISM_pow_exp_01_EMULATOR.RData ./inst/extdata/GSAT/bamber19."$ssp".temperature.fair.temperature_climate.nc $ssp ./out/GIS_ALL_"$ssp"_"$final_year"/ 2024 GIS_ALL_"$ssp"_"$final_year"
  fi
  if [ "$final_year" == 2100 ]
  then
    ./emulandice_steer.sh GIS ALL ./data-raw/GIS_ALL_CISM_IMAUICE_ElmerIce_GISM_pow_exp_01_EMULATOR.RData ./inst/extdata/GSAT/bamber19."$ssp".temperature.fair.temperature_climate.nc $ssp ./out/GIS_ALL_"$ssp"_"$final_year"/ 2024 GIS_ALL_"$ssp"_"$final_year"
  fi

  done

# Won't copy if predictions exist
mkdir $outdir
mv "$emulandice_dir"/out/GIS* "$emulandice_dir"/data-raw/GIS* $outdir

