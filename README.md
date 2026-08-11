# emulandice2
Emulator of multi-model land ice projections: emulandice v2 for FACTS

# Setup

- Install R and cmake on your system;

- Install package dependencies, e.g., by pasting the following into the R console:

```
install.packages('mvtnorm')    # multivariate normal for emulator uncertainties
install.packages('RcppEigen')  # needed for RobustGaSP
install.packages("nloptr")     # needed for RobustGaSP
install.packages('RobustGaSP') # emulator package
install.packages('ncdf4')      # for reading and writing netcdfs
install.packages('config')     # for using YAML configuration files
```

- Clone this repository;

- Install emulandice2. From the parent directory of the cloned repository, run:

```
R CMD INSTALL --no-multiarch --with-keep.source emulandice2
```



# 1. BUILD: OPTIMISE EMULATOR IN PROTECT 

**Runs top-level script: emulator_build.R** 

This is slow! The emulators will be optimised and validated during the PROTECT project, then fixed for use in FACTS. This means no new simulations can be added from that point, as this would require retraining and re-validation of the emulators.

Current commands:

### Glaciers, region 3: 2300 

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GLA 3 2300`

This can be run for any region number from 1 to 19, and for final year of 2100 or 2300.

### Antarctica: 2300 

`Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" AIS ALL 2300`

This can be run for ALL (whole ice sheet), or sectors WAIS, EAIS or PEN (west, east, peninsula), and 
for final year of 2100, 2150 or 2300.

### Greenland: 2300 or 2100

```
Rscript --vanilla -e "library(emulandice2)" -e "source('emulator_build.R')" GIS ALL 2300
```

This can also be run for a final year of 2100.

Other final years may also work - see run shell script (run_*.sh) or just try. If no arguments are used, the code should default to running something quick for testing.

**Primary output file:**

Each command generates an *_EMULATOR.RData file, called something like:
GLA_RGI03_GloGEM_OGGM_pow_exp_20_EMULATOR.RData

This name concatenates the ice source and region (GLA_RGI03), the list of models the GP emulator was trained on (GloGEM_OGGM), and the GP emulator covariance (here power exponential, alpha = 2.0). The file contains the emulator object as well as many other variables used for prediction in FACTS (below).


**Other output files:**

* [filename]_build.txt - main log file 
* [filename]_*.log - log file dump from RobustGaSP or other GP
* [filename]_SIMS.pdf - plots of simulations
* [filename]_SA.pdf - plots of sensitivity analysis
* [filename]_VALID*.pdf - plots of train & test validation in selected years
OR
* [filename]_LOO*.pdf - plots of leave-one-out validation in selected years
* [filename]_region_fractions.pdf - plots of ice sheet fractional contributions, if enabled

Output files are written to outdir, which is currently hard-coded as "~/PROTECT/RESULTS/tmp"). 

### Build YAML options

Build settings are read from `inst/config_{ice_source}_{region}.yml` (e.g. `inst/config_GLA_RGI03.yml`). Keys omitted from a region file use the code defaults below. **`emulator_type` and `emulator_covar` are mandatory** (no code default). Full template with every key: `inst/config_GLA_RGI03.yml`.

| Key | Default if omitted | Notes |
|-----|--------------------|--------|
| `sims_only` | `FALSE` | Read/filter/plot sims only |
| `deliverable_test` | `FALSE` | PROTECT deliverable repro settings |
| `validation_type` | `"tvt"` | `"tvt"` or `"loo"` |
| `n_train` | `1000` | TVT training cap |
| `emulator_type` | — **required** | e.g. `"statGP"`, `"laGP"` |
| `emulator_covar` | — **required** for statGP/dgpsi | e.g. `"pow_exp_01"` |
| `temp_anom_type` | `"baseline"` | `"baseline"` or `"relative"` |
| `temp_nyrs` | `30` | GSAT averaging window |
| `temp_end_years` | ice-source-specific | First value = baseline end; rest = timeslices |
| `plot_level` | `2` | `0` none, `1` main, `2` exhaustive |
| `do_history_match` | `TRUE` | Pre-screen ensemble vs observations |
| `scale_mod_err_sel` | `5.0` | History-match model discrepancy × obs error | Must be >= 0
| `scree_thresh` | `0.999` | SVD threshold; if unset, GIS ≥ 2200 uses `0.99999` |
| `glacier_data` | `"GlaMBIE"` | GLA only (`"GlaMBIE"` / `"Hugonnet"`) |
| `complete_thresh_oggm` | `0.90` | GLA only |
| `complete_thresh_go` | `0.85` | GLA only |

Resolved values are written to `*_build.txt` at startup. Plotting uses `plot_level` from YAML (default `2`).


# 2. PREDICT: RUN EMULATOR IN FACTS 

**Runs top-level script: main.R**

Once the GP emulator build files are created, they can be used in FACTS to quickly predict land ice contributions to sea level for a set of GSAT projections by running the steering shell script like this:

`./emulandice_steer.sh ice_source region path_to_emu_file path_to_climate_data_file scenario outdir seed_num pipeline_id`

* ice_source: GIS, AIS, or GLA 
* region: ALL (for GIS or AIS); WAIS, EAIS or PEN (for AIS); RGI01 to RGI19 (for GLA; note zero-padding)
* path_to_emu_file: this is the full path and name of the .RData build file created in stage 1; the build file name must be called "ice_source_region_*.RData"
* path_to_climate_data_file: this is the full path and name of the global temperature netcdf file used to drive FACTS projections
* scenario: e.g. ssp126, ssp585 (must be in the climate data file and recognised by emulandice)
* outdir: location of projections output directory (will be written if it does not exist)
* seed_num: random seed number, for controlling random sampling
* pipeline_id: name for this set of projections (defined by FACTS or user and used in the output netcdf filenames)

The emulandice_steer.sh file generates predictions by running main.R using the above arguments:

`Rscript --vanilla -e "library(emulandice2)" -e "source('main.R')" $ice_source $region $emu_file $climate_data_file $scenario $outdir $seed $pipeline_id`

The steering bash script and module will do some basic checks on these arguments, and attempt to write the output directory if it does not exist.

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

All have one row per GSAT projection.


