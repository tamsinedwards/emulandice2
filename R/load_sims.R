#' load_obs: climate forcings and land ice projections.
#'
#' @description
#' Load current CSV file for climate, ice sheet or glacier simulations.
#'
# @param dataset Dataset: "PROTECT" or "IPCC_AR6" - obsolete
#' @param variable Variable: "climate" or "ice
#' @param source Ice source, i.e. value of i_s, if variable is ice
#' @param region Region - currently only used for GLA
#'
#' @returns `load_sims()` returns CSV of dataset
#'
#' @export

load_sims <- function(variable, source = NA, region = NA) { # dataset

#  stopifnot(dataset %in% c("IPCC_AR6", "PROTECT"))
  stopifnot(variable %in% c("climate","ice"))

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat(paste("load_sims: reading",variable,"simulation data\n"), # dataset
      file = logfile_build, append = TRUE)

#  if (dataset == "IPCC_AR6") {
#    if (variable == "climate") data_file <- paste0( inputs_ext, "IPCC_AR6/20210215_CLIMATE_FORCING_IPCC.csv")
#    if (variable == "ice") data_file <- paste0( inputs_ext, "IPCC_AR6/20201106_SLE_SIMULATIONS.csv")
#  }

#  if (dataset == "PROTECT") {

    # Climate forcing simulations put together by Mira
    # xxx Mira email 9th May about 230508 file: small discrepancies HadGEM2-ES Cecile
    # xxx Hatchet job 230618 merging Vio's 2300 file missing data
    if (variable == "climate") data_file <- paste0( inputs_preprocess, "/GSAT/CLIMATE_FORCING_240127.csv")

    # Land ice simulations: results from PROTECT!
    # GLA are in mm, ice shets are cm... xxx put this in filename when fixing abs values
    if (variable == "ice") {
      if (source == "GIS") data_file <- paste0( inputs_preprocess, "/GIS/SLE_SIMULATIONS_GIS_p9_240210.csv") # 240317 has slc not sle
      if (source == "AIS") data_file <- paste0( inputs_preprocess, "/AIS/SLE_SIMULATIONS_AIS_full_ZWALLY00_240306.csv")
      if (source == "GLA") {
        # All regions in one file when not many runs
        # data_file <- paste0( inputs_preprocess, "/GLA/SLE_SIMULATIONS_GLA_v2_240317.csv")

        data_file <- paste0( inputs_preprocess, "/GLA/SLE_SIMULATIONS_GLA_",region,"_240821.csv")

      }
    }

#  }

  cat(paste("load_sims: read", data_file, "\n"), file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)


  return(read.csv(data_file))


}
