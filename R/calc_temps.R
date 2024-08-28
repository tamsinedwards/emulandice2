#' calc_temps: calculate climate change metric for emulator input
#'
#' @description
#' Calculate climate change metric(s) for emulator input from climate forcings
#'
#' @returns `calc_temps()` returns requested temperature change(s) for all
#' simulations in climate forcing file.
#'
#' @export

calc_temps <- function(climate_dataset) {

  # Baseline period
  temps_period <- temps_baseline + 1:N_temp_yrs - 1

  cat( paste("GSAT: baseline mean period", paste(range(temps_period), collapse = "-"), "\n"),
       file = logfile_build, append = TRUE )

  # Drop scenario, vals before calculating just to save indexes (re-add after)
  climate_vals <- climate_dataset[ , -c(1,2) ]

  # Normalise by subtracting baseline mean from whole time series
  climate_norm <- t( apply(climate_vals, 1, function(x) {
                          x <- unlist(x) - mean( unlist(x[ paste0( "y", temps_period) ]) )
                          }) )

  # Calculate temp change(s) we want for emulator
  # Either vector or array, depending on how many GSAT timeslices requested
  if ( length(temps_list) == 1 ) {

    temps_period_future <- temps_list - N_temp_yrs:1 + 1

    cat( paste("GSAT: future mean period", paste(range(temps_period_future), collapse = "-"), "\n"),
         file = logfile_build, append = TRUE )

    climate_anom <-  apply(climate_norm, 1, function(x) {
      x <- mean( unlist( x[ paste0( "y", temps_period_future) ] ) ) })


  } else {

    climate_anom <- matrix( NA, nrow = dim(climate_norm)[1], ncol = length(temps_list) )

    for ( tt in 1:length(temps_list)) {

      temps_period_future <- temps_list[tt] - N_temp_yrs:1 + 1

      cat( paste("GSAT: future mean period", paste(range(temps_period_future), collapse = "-"), "\n"),
           file = logfile_build, append = TRUE )

      climate_anom[ , tt] <- apply(climate_norm, 1, function(x) {
        mean( unlist(x[ paste0( "y", temps_period_future) ]) )
      })

    }

  }

  # Add scenario and GCM columns back
  climate_anom <- cbind( climate_dataset[ , 1:2 ], climate_anom )
  colnames(climate_anom)[-c(1,2)] <- paste0("GSAT_", temps_list)

  return(climate_anom)

}
