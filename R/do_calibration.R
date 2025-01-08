#' do_calibration: calculate calibration information.
#'
#' @description
#' Use distance of each emulator prediction over the historical period from
#' observations to calculate normalised weights using a Gaussian likelihood (i.e. Bayesian calibration).
#'
#' @param distances Distance of each historical emulator prediction from the
#' observations
#'
#' @returns `do_calibration()` returns weights for each ensemble member
#'
#' @export

do_calibration <- function(distances) {

  # Calculate weights with Gaussian univariate likelihood
  weightsraw <- sapply( distances, function(x) exp( -0.5 * sum( x * x , na.rm=TRUE) / (obs_change_err * obs_change_err) ) )
  weights <- weightsraw / sum( weightsraw )

  return(weights)

}
