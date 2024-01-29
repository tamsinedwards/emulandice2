# emulandice2
Emulator of multi-model land ice projections: emulandice v2 for FACTS

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

`./emulandice_steer.sh ice_source region path_to_emu_file path_to_climate_data_file scenario outdir seed_num pipeline_id`

The emulandice_steer.sh file contains the command:

`Rscript --vanilla -e "library(emulandice2)" -e "source('main.R')" $ice_source $region $emu_file $climate_data_file $scenario $outdir $seed $pipeline_id`

where the arguments are taken from the command line. The first two define the ice source and region, 
which must match those in the filename of the third (emu_file). The rest give the full names and paths of the emulator build and climate data files, the scenario, path to output directory, random seed, and pipeline_id to prepend to the netcdf filename. 

The steering bash script and module will do some basic checks on these arguments, and attempt to write the output directory if it does not exist.

The current builds are dated 28th-29th January 2024. [need to find a place to put these]

If no arguments are used, the code should default to running something quick for testing.

**Primary output file:**

The projections are written in FACTS netcdf format.

**Other output files:**

Output files are written to $outdir specified on the command line (see above).

* [filename]_results.txt - main log file 
* [filename]_RESULTS.Rdata - workspace file

Plotting is turned OFF by default. Changing plot_level from 0 to 1 or 2 will output:

* [filename]_UNCALIBRATED.pdf - plots of uncalibrated (prior) projections
* [filename]_CALIBRATED.pdf - plots of calibrated (posterior) projections

Outputting CSV files is switched OFF by default. To turn it on, set write_csv = TRUE.

* [filename]_projections_MEAN_SSPXXX.csv - uncalibrated mean projections (i.e. no emulator uncertainty - just for information).
* [filename]_projections_FULL_SSPXXX.csv - uncalibrated full projections (i.e. with emulator uncertainties)
* [filename]_projections_POSTERIOR_SSPXXX.csv - Bayesian calibrated projections 
* [filename]_projections_NROY_SSPXXX.csv - history matching calibrated projections (NROY = not ruled out yet). 

All have one row per GSAT projection, except NROY where some are usually ruled out.


