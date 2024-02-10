#' calculate_sle_anom: calculate sea level changes.
#'
#' @description
#' Calculate sea level changes (cm SLE) relative to a baseline year.
#' XXX This function will be obsolete if I standardise SLE file units and
#' baselines.
#'
#' @returns `calculate_sle_anom()` returns ice_data with SL columns in
#' corrected units and relative to baseline year.
#'
#' @export

calculate_sle_anom <- function() {

#  stopifnot(dataset == "PROTECT")

#  if (dataset == "PROTECT") {

    cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
    cat("calculate_sle_anom: calculate SLE anomalies and standardise units\n",file = logfile_build, append = TRUE)

    # CALCULATE SEA LEVEL ANOMALIES w.r.t. calibration start date
    ice_data[ , paste0("y",years_sim)] <- ice_data[ , paste0("y",years_sim)] - ice_data[ , paste0("y",cal_start) ]

    # Convert mm SLE volume (above flot for GloGEM; also OGGM?) to cm SLE for glaciers
    if (i_s == "GLA") ice_data[ , paste0("y",years_sim)] <- ice_data[ , paste0("y",years_sim) ] / 10.0

    # Convert m SLE to cm SLE for ice sheets
    # Make CSVs consistent instead? xxx
    if (i_s %in% c("GIS", "AIS")) ice_data[ , paste0("y",years_sim)] <- ice_data[ , paste0("y",years_sim) ] * 100.0

#  }

  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  return(ice_data)

}
