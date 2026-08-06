# Calculate climate change metric(s) i.e. GSAT timeslices
#
# Common function gsat_anom_row() to calculate GSAT timeslices
# from GCMs (for training) or FaIR annual timeseries (for prediction)
#
# Calculates mean baseline and one or more future time means,
# either relative to baseline or relative to previous timeslice [2nd not yet implemented]

# Year index for N-year windows ending at end_year
gsat_window_years <- function(end_year, n_yrs) {
  (end_year - n_yrs + 1L):end_year
}

# Mean of GSAT annual columns for one row (named vector or 1 row dataframe)
gsat_window_mean <- function(x, end_year, n_yrs) {
  mean(unlist(x[paste0("y", gsat_window_years(end_year, n_yrs))]))
}

# GSAT anomalies vs baseline or previous period, for one or more end years
# Always length(end_years)
# Called by calc_temps_gcms() for training and by load_design_to_pred() for FaIR design
gsat_anom_row <- function(x, end_years, baseline_end, n_yrs, anom_type) {

  # Calculate baseline mean
  base <- gsat_window_mean(x, baseline_end, n_yrs)

  # Calculate future mean periods relative to baseline
  abs_anom <- vapply(end_years, function(tt) gsat_window_mean(x, tt, n_yrs) - base, numeric(1))

  # Return anomalies all relative to baseline, or relative to each previous time period
  if (anom_type == "baseline") { abs_anom
  } else c(abs_anom[1], diff(abs_anom))

}



# Calculate baseline

# calc_temps() code
# temps_period <- (temps_baseline - N_temp_yrs + 1):temps_baseline
#
# # load_design_to_pred() code
# temps_period1 <- (temps_baseline - N_temp_yrs + 1):temps_baseline
#
# # One GSAT timeslice (unlikely now, but still need check)
# if ( length(temps_list) == 1 ) {
#
#   # calc_temps() code
#   temps_period_future <- temps_list - N_temp_yrs:1 + 1
#   climate_anom <-  apply(climate_norm, 1, function(x) {
#     x <- mean( unlist( x[ paste0( "y", temps_period_future) ] ) ) })
#
#   # load_design_to_pred() code
#   temps_period2 <- temps_list - N_temp_yrs:1 + 1
#   design_prior_gsat[ ss ] <- mean(unlist(climate_prior[ ss, paste0("y", temps_period2) ])) - mean(unlist(climate_prior[ ss, paste0("y", temps_period1) ]))
#
#
# } else { # Multiple GSAT timeslices
#
#   # calc_temp() code
#   climate_anom <- matrix( NA, nrow = dim(climate_norm)[1], ncol = length(temps_list) )
#   for ( tt in 1:length(temps_list)) {
#     temps_period_future <- temps_list[tt] - N_temp_yrs:1 + 1
#     climate_anom[ , tt] <- apply(climate_norm, 1, function(x) {
#       mean( unlist(x[ paste0( "y", temps_period_future) ]) )
#     })
#   }
#
#   # load_design_to_pred() code
#   for ( tt in temps_list ) {
#     temps_period2 <- tt - N_temp_yrs:1 + 1
#     design_prior_gsat[ ss, paste0("y", tt) ] <- mean(unlist(climate_prior[ ss, paste0("y", temps_period2) ])) - mean(unlist(climate_prior[ ss, paste0("y", temps_period1) ]))
#   }
#
# }


