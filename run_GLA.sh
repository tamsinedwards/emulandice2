#!/bin/bash
# Run GLA analysis
# ./run_GLA.sh final_year
# where final_year is 2100 or 2300

# Check plot_level = 1 in main.R

final_year=$1

if [ "$final_year" != 2100 -a "$final_year" != 2300 ]
then
     echo "Incorrect final year argument: please choose from 2100 or 2300"
     exit 1
fi


now=$(date +'%y%m%d')
outdir=/Users/tamsinedwards/PROTECT/RESULTS/"$now"_GLA_ALL_"$final_year"

for region in $(seq -f "%02g" 1 19)
do

  echo "Build file for region RGI: $region"
  Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GLA $region $final_year

  echo "Predict for region RGI: $region"

  for ssp in "ssp126" "ssp245" "ssp585"
  do

  echo "Scenario:" $ssp

  if [ "$region" == "01" -o "$region" == "04" -o "$region" == "05" -o "$region" == "07" -o "$region" == "19" ]
  then
       covar="pow_exp_01"
  else
       covar="pow_exp_20"
  fi
  ./emulandice_steer.sh GLA RGI"$region" ./data-raw/GLA_RGI"$region"_GloGEM_OGGM_"$covar"_EMULATOR.RData ./inst/extdata/GSAT/bamber19."$ssp".temperature.fair.temperature_climate.nc $ssp ./out/GLA_RGI"$region"_"$ssp"_"$final_year"/ 2024 GLA_RGI"$region"_"$ssp"_"$final_year"

  done
done

# Won't copy if predictions exist
mkdir $outdir
mv /Users/tamsinedwards/PROTECT/emulandice2/out/GLA* ~/PROTECT/emulandice2/data-raw/GLA* $outdir
