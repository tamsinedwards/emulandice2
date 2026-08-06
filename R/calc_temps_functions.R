# Calculate climate change metric(s) i.e. GSAT timeslices
#
# Common function gsat_anom_row() to calculate GSAT timeslices
# from GCMs (for training) or FaIR annual timeseries (for prediction)
#
# Calculates mean baseline and one or more future time means,
# relative to baseline
#
# Also a function to calculate these relative to previous timeslice

# Year index for N-year windows ending at end_year
gsat_window_years <- function(end_year, n_yrs) {
  (end_year - n_yrs + 1L):end_year
}

# Mean of GSAT annual columns for one row (named vector or 1 row dataframe)
gsat_window_mean <- function(x, end_year, n_yrs) {
  mean(unlist(x[paste0("y", gsat_window_years(end_year, n_yrs))]))
}

# GSAT anomalies vs baseline, for one or more end years
# Always length(end_years)
# Called by calc_temps_gcms() for training and by load_design_to_pred() for FaIR design
gsat_anom_row <- function(x, end_years, baseline_end, n_yrs) {

  # Calculate baseline mean
  base <- gsat_window_mean(x, baseline_end, n_yrs)

  # Calculate future mean periods relative to baseline
  vapply(end_years, function(tt) gsat_window_mean(x, tt, n_yrs) - base, numeric(1))

}

# Return diffs relative to each previous time slice
gsat_abs_to_relative <- function(M) {

  col_M <- colnames(M)
  M <- cbind(M[, 1, drop = FALSE], t(diff(t(as.matrix(M)))))
  colnames(M) <- col_M

  M

}


