#' emulator_uncertainty: sample from emulator uncertainties
#'
#' @description Generate final projections from the mean by sampling emulator
#' uncertainty once for each (i.e. same size dataset)
#'
#' @param myem_scen The emulator projections for a given scenario (contains
#' both the mean and variance).
#'
#' @export

emulator_uncertainty <- function(myem_scen) {

  # Number of projections per SSP
  # This will be equal to N_prior for unif_temps design (set by user in emulator_build.R)
  # and N_temp for AR6_2LM (deduced from FaIR netcdf file in main.R)
  N_samp <- dim(myem_scen$mean)[1]

  # Initialise
  proj_uncert <- matrix( nrow = N_samp, ncol = N_ts)
  colnames(proj_uncert) <- paste0("y", years_em)

  # For each GSAT projection
  for (ss in 1:N_samp){

    # FACTS: if mvtnorm package is a pain, could use another
    # VARIANCE MATRIX: run type = "var" for predict. Can get quite wiggly!
    # xxx Replace normal with t?
    proj_uncert[ss, ] <- mvtnorm::rmvnorm(n = 1, mean = myem_scen$mean[ss, ], sigma = myem_scen$var[ss, , ])

  }

  return(proj_uncert)

}
