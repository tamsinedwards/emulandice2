# emulandice2
Emulator of multi-model land ice projections: emulandice v2

# Setup

Load R and cmake on your system.

Install package dependencies:

```
install.packages('mvtnorm') - multivariate normal for emulator uncertainties
install.packages('RcppEigen') - needed for RobustGaSP, I think
install.packages("nloptr") - needed for RobustGaSP
install.packages('RobustGaSP') - emulator package
install.packages('ncdf4') - for reading and writing netcdfs
```

Once emulandice2 is in CRAN, these dependencies will be automatically installed with the module (using install.packages()).


Install emulandice2:

`R CMD INSTALL --no-multiarch --with-keep.source emulandice`



# 1. BUILD: OPTIMISE EMULATOR IN PROTECT 

**Runs top-level script: emulator_build.R** 

This is slow! The emulators will be optimised and validated during the PROTECT project, then fixed for use in FACTS. This means no new simulations can be added from that point, as this would require retraining and re-validation of the emulators.

Current commands:

### Glaciers, region 3 of 19: 2300 

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GLA 3 2300`

### Antarctica: 2300 

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" AIS 0 2300`

### Greenland: 2300 or 2100

```
Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS 0 2300
Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS 0 2100
```

If no arguments are used, the code should default to running something quick for testing (currently: glaciers, region 3).

**Primary output file:**

Each command generates an *_EMULATOR.RData file, called something like:
GLA_RGI03_GloGEM_OGGM_pow_exp_20_EMULATOR.RData

This name concatenates the ice source and region (GLA_RGI03), the list of models the GP emulator was trained on (GloGEM_OGGM), and the GP emulator covariance (here power exponential, alpha = 2.0). The file contains the emulator object as well as many other variables used for prediction in FACTS (below).


**Other output files:**

* [filename]_build.txt - main log file 
* [filename]_rgasp.log - log file dump from RobustGaSP
* [filename]_SIMS.pdf - plots of simulations
* [filename]_SA.pdf - plots of sensitivity analysis

Output files are written to outdir, which is currently hard-coded as "~/PROTECT/RESULTS/tmp"). 

Plotting is turned ON by default. Changing plot_level from 2 to 1 will reduce the number of plots, and to 0 will prevent either pdf file being made.


# 2. PREDICT: RUN EMULATOR IN FACTS 

**Runs top-level script: main.R**

Once the GP emulator build files are created, they can be used in FACTS to quickly predict land ice contributions to sea level for a set of GSAT projections:

`./emulandice_steer.sh ice_source region models_covariance climate_data_file scenario`

The emulandice_steer.sh file contains the command:

`Rscript --vanilla -e "library(emulandice2)" -e "source('main.R')" $ice_source $region $emu_name $climate_data_file $scenario`

where the arguments are taken from the command line. The first three form the 
component parts of the RData file, as described above, and the last two give the climate data file and scenario.

The current commands (from some basic optimisation checks using preliminary data in autumn 2023) for SSP5-8.5 are:

```
./emulandice_steer.sh GLA RGI03 GloGEM_OGGM_pow_exp_20 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
./emulandice_steer.sh AIS ALL Kori_PISM_pow_exp_10 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
./emulandice_steer.sh GIS ALL CISM_IMAUICE_pow_exp_20 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
./emulandice_steer.sh GIS ALL CISM_pow_exp_20 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
```

If no arguments are used, the code should default to running something quick for testing (currently: glaciers, region 3).

**Primary output file:**

The projections are written in FACTS format, e.g. for SSP5-8.5:

emulandice.ssp585.emuGLA.emulandice.GLA_RGI03_globalsl.nc.

**Other output files:**

* [filename]_results.txt - main log file 
* [filename]_RESULTS.Rdata - workspace file
* [filename]_projections_FULL_SSPXXX.csv - CSV of uncalibrated final projections

Output files are written to outdir, which is currently taken from the emulator build 
file ("~/PROTECT/RESULTS/tmp"). 

Plotting is turned OFF by default. Changing plot_level from 0 to 1 or 2 will output:

* [filename]_UNCALIBRATED.pdf - plots of uncalibrated (prior) projections
* [filename]_CALIBRATED.pdf - plots of calibrated (posterior) projections

Outputting mean projections (without emulator uncertainty) is switched OFF by default. 
To turn it on, set write_mean = TRUE.

* [filename]_projections_MEAN_SSPXXX.csv - CSV of uncalibrated mean projections.

