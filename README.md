# emulandice_test
Emulator of multi-model land ice projections (v2 of emulandice) - test space

# Setup

Load R and cmake on your system.

Install package dependencies:

```
install.packages('RcppEigen') (I think ?)
install.packages('mvtnorm')
install.packages("nloptr")
install.packages('RobustGaSP')
install.packages('ncdf4')
```

Once emulandice2 is in CRAN, these dependencies will be automatically installed with the module (using install.packages()).


Install emulandice2:

`R CMD INSTALL --no-multiarch --with-keep.source emulandice`



# BUILD: OPTIMISE EMULATOR IN PROTECT 

** Runs top-level script: emulator_build.R ** 

This is slow! The idea is that the emulators will be optimised and validated during PROTECT and publication, then fixed for FACTS. This means no new simulations can be added from that point, as this would require retraining and re-validation.

Current commands:

### Glaciers, region 3 of 19: 2300 

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GLA 3 2300`

### Antarctica: 2300 

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" AIS 0 2300`

### Greenland: 2300 or 2100

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS 0 2300`
`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS 0 2100`

Each command generates an *_EMULATOR.RData file, called something like:
GLA_RGI03_GloGEM_OGGM_pow_exp_20_EMULATOR.RData

This name concatenates the ice source and region (GLA_RGI03), the list of models the GP emulator was trained on (GloGEM_OGGM), and the GP emulator covariance (here power exponential, alpha = 2.0). The file contains the emulator object as well as many other variables used for prediction (below).



# PREDICT: RUN EMULATOR IN FACTS 

**Runs top-level script: main.R**

Once the GP emulators are fixed, they can be used in FACTS to quickly predict land ice contributions to sea level for a set of GSAT projections:

`./emulandice_steer.sh ice_source region models_covariance`

The emulandice_steer.sh file contains the command:

`Rscript --vanilla -e "library(emulandice2)" -e "source('main.R')" $ice_source $region $emu_name`

where the final three arguments are taken from the command line. These form the component parts of the RData file as described above.

The current commands (from some basic optimisation checks using preliminary data in autumn 2023) for SSP5-8.5 are:

```
./emulandice_steer.sh GLA RGI03 GloGEM_OGGM_pow_exp_20 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
./emulandice_steer.sh AIS ALL Kori_PISM_pow_exp_10 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
./emulandice_steer.sh GIS ALL CISM_IMAUICE_pow_exp_20 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
./emulandice_steer.sh GIS ALL CISM_pow_exp_20 emulandice.ssp585.temperature.fair.temperature_climate.nc ssp585
```



