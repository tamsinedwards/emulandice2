#' load_obs: climate forcings and land ice projections.
#'
#' @description
#' Load current CSV file for climate, ice sheet or glacier simulations.
#'
#' @param variable Variable: "climate" or "ice
#' @param source Ice source, i.e. value of i_s, if variable is ice
#' @param region Region - currently only used for GLA
#'
#' @returns `load_sims()` returns CSV of dataset
#'
#' @export

load_sims <- function(variable, source = NA, region = NA) { # dataset

  stopifnot(variable %in% c("climate","ice"))

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat(paste("load_sims: reading",variable,"simulation data\n\n"), # dataset
      file = logfile_build, append = TRUE)

    # Climate forcing simulations put together by Mira
    # xxx Mira email 9th May 2023 about 230508 file: small discrepancies HadGEM2-ES Cecile
    # xxx Hatchet job 230618 merging Vio's 2300 file missing data
    if (variable == "climate") {
      data_file <- paste0( inputs_preprocess, "/GSAT/CLIMATE_FORCING_251030.csv")
      if (deliverable_test) data_file <- paste0( inputs_preprocess, "/GSAT/CLIMATE_FORCING_240127.csv")

      # Read csv
      data_csv <- read.csv(data_file)
    }

    # Land ice simulations from PROTECT
    # GLA are in mm SLE, ice sheets are m SLE (units in file name)
    if (variable == "ice") {

      if (source == "GIS") {
        data_file <- paste0( inputs_preprocess, "/GIS/SLE_SIMULATIONS_GIS_m_2014_250719.csv" )
        if (deliverable_test) data_file <- paste0( inputs_preprocess, "/GIS/SLE_SIMULATIONS_GIS_p9_240210.csv")
      }

      if (source == "AIS") {
        data_file <- paste0( inputs_preprocess, "/AIS/AIS_SIMULATIONS_", region, "_m_SLE_2014_260327.csv")
        if (deliverable_test) data_file <- paste0( inputs_preprocess, "/AIS/SLE_SIMULATIONS_AIS_full_ZWALLY00_240306.csv")
      }

      if (source == "GLA") {

        # Regional file
        data_file <- paste0( inputs_preprocess, "/GLA/GLA_SIMULATIONS_",region,"_mm_SLE_2014_260604.csv")

        # All regions in one file when there were not many runs
        if (deliverable_test) data_file <- paste0( inputs_preprocess, "/GLA/SLE_SIMULATIONS_GLA_v2_240317.csv")

      }

      # Read csv
      data_csv <- read.csv(data_file)

      # Convert ice simulations to same units, assuming glacier CSV file is in mm and ice sheet files are in m
      cat(paste("load_sims: convert to cm SLE\n"), file = logfile_build, append = TRUE)

      # Convert mm to cm SLE for glaciers
      if (source == "GLA") data_csv[ , paste0("y",years_sim)] <- data_csv[ , paste0("y",years_sim) ] / 10.0

      # Convert m to cm SLE for ice sheets
      if (source %in% c("GIS", "AIS")) data_csv[ , paste0("y",years_sim)] <- data_csv[ , paste0("y",years_sim) ] * 100.0

    } # ice

  cat(paste("load_sims: read file", data_file, "\n"), file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  return(data_csv)


}
