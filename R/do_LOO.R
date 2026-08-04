#' do_LOO: calculate leave-one-out information.
#'
#' @description
#' Build emulator on all but one simulation and calculate difference with that
#' simulation. Repeat for list of all or a subset of the ensemble.
#'
#' @param designX Full dataset design
#' @param responseF Full dataset response
#' @param forcingX Full forcing timeseries (if SVD)
#' @param year_list List of projection years to do the LOO for.
#' @param N_k Repeat the LOO for every N_k-th simulation (NA for all)
#'
#' @returns `do_loo()` returns mean and sd of each test for each year.
#'
#' @export

do_loo <- function(designX, responseF, forcingX, year_list, N_k = NA) {

  cat("_____________________________________\n",file = logfile_build, append = TRUE)
  cat(paste("do_loo:", paste(year_list, collapse = ","),"\n"), file = logfile_build, append = TRUE)

  # Limit to this number of cores for laptop
  n_cores <- 32L

  cat(paste("Number of cores:", n_cores,"\n"), file = logfile_build, append = TRUE)
  options(mc.cores = n_cores)

  mean <- matrix( ncol = length(year_list), nrow = N_sims)
  sd <- matrix( ncol = length(year_list), nrow = N_sims)

  colnames(mean) <- paste0( "y", year_list)
  colnames(sd) <- paste0( "y", year_list)

  # xxx Change to stratified sample?
  if (is.na(N_k) ) {
    sim_list <- 1:N_sims
    cat(paste("\nDoing LOO for all",length(sim_list), "simulations...\n"),file = logfile_build, append = TRUE)
  } else {
    sim_list <- seq(from = 1, to = N_sims, by = N_k)
    cat(paste0("\nDoing LOO for subset of ",length(sim_list),
               " simulations (1 in ", N_k, ")...\n\n"), file = logfile_build, append = TRUE )
  }


  # LOO function for one simulation (to parallelise)
  loo_test <- function(ss) {

    emu_mv_loo <- NA

    # Fit emulator to all but that one simulation
    # 22/7/25: changed to be X and Y from emulator_build.R passed as arguments
    if (temp_input == "mean") emu_mv_loo <- try(emulandice2::make_emu( designX = designX[ -ss, ],
                                                          responseF = responseF[ -ss, paste0("y", years_em) ],
                                                          thresh = scree_thresh ) )
    # Skip if failed
    # xxx This was for GIS CISM while trying to understand error
    # xxx Replaced 'next' with warning when changed to function - put back?
    if (inherits(emu_mv_loo, "try-error")) warning("Failed to do LOO test") # next

    # Predict for this one year - so no need for correlation between timeslices
    # This mimics emulator_predict() function, but asks to return just mean and sd, not var
    if (temp_input == "mean") emu_one <- emu_mv_loo( designX[ ss, ], type = "sd")
    colnames(emu_one$mean) <- paste0("y", years_em)
    colnames(emu_one$sd) <- paste0("y", years_em)

    emu_one

  } # function

  # Parallel LOO
  # Returns list of tests
  emu_all <- parallel::mclapply(sim_list, loo_test)

  # Put each test into matrix of N_sims rows

  # XXX CHANGE TO RETURN ALL FOR PLOTS

  for (ii in 1:length(sim_list)) {

    ss <- sim_list[ii]
    loo_ind <- which(years_em %in% validation_years, arr.ind = TRUE)

    mean[ss, ] <- emu_all[[ii]]$mean[ loo_ind ]
    sd[ss, ] <- emu_all[[ii]]$sd[ loo_ind ]
  }

  # Mean and sd are matrices
  return( list(mean = mean, sd = sd))

}


