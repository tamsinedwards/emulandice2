#' load_design_to_pred: construct design for emulator projections
#'
#' @description
#' Construct requested design for emulator projections by sampling inputs.
#'
#' @param design_name Requested design: SSPs, uniform temperatures over original
#' ensemble ranges, or main effects.
#'
#' @param N_samp   # Number of bins per axis for continuous parameters
#'
#' # Default is used for main effects; passed value is used for fixed_temp
#' For AR6_2LM sample size is set by number of FaIR samples

#' @returns `load_design_to_pred()` returns requested designs for each list
#' member (SSP, or input for main effects).
#'
#' @export

load_design_to_pred <- function(design_name, N_samp = NA) {

  # Prediction designs
  # Main_effects and unif_temps are used by emulator_build.R for emulator validation,
  # fixed_temp is used for calibrating ice sheet model parameters for fixed GSAST,
  # and AR6_2LM is used by main.R to predict
  stopifnot(design_name %in% c("main_effects", "unif_temps", "fixed_temp", "AR6_2LM"))

  # Must specify sample size, or else if FaIR prior cannot specify as read from climate netcdf
  stopifnot( !is.na(N_samp) || (design_name == "AR6_2LM" && is.na(N_samp)) )

  # Set this for calls from emulator_build.R; set in main.R for projections designs
  if ( design_name %in% c("main_effects", "unif_temps") ) prior_choices <- "uniform"

  if (design_name %in% c("main_effects", "unif_temps")) { logfile_design <- logfile_build
  } else logfile_design <- logfile_results

  cat("\n_________________________________\n",file = logfile_design, append = TRUE)
  cat(paste("load_design_pred:", design_name, "\n\n"), file = logfile_design, append = TRUE)

  # One matrix for each scenario (uniform GSAT and AR6 prior)
  # or each input (main effects)
  design_prior <- list()

  # Loop over design columns
  # This includes dummy variables i.e. expanded factors
  input_names <- colnames(ice_design)

  # Range of prior: mostly will be uniform
  prior_range <- list()
  for ( pp in ice_cont_list ) prior_range[[pp]] <- c(NA, NA)

  # Greenland retreat ----------------------------------------------------------

  # SET EMPIRICAL PRIOR FOR Greenland
  if (i_s == "GIS") {

    if ("retreat" %in% ice_param_list) {

      cat("Prior for retreat: empirical (Slater et al.)\n", file = logfile_design, append = TRUE)

      # K-distribution from Donald Slater (N = 191)
      # Sample from kernel density estimate of this
      # using same bandwith as original study

      # This was called "data_for_tamsin.txt" in emulandice for IPCC AR6:
      #k_dist_file <- system.file("extdata", "data_for_tamsin.txt", package = "emulandice2", mustWork = TRUE )
      #k_dist_file <- paste0(inputs_ext, "GIS/retreat/GIS_retreat_prior.txt" )
      k_dist_file <- system.file("extdata", "/GIS/retreat/GIS_retreat_prior.txt", package = "emulandice2", mustWork = TRUE )
      cat(paste(k_dist_file, "\n"), file = logfile_design, append = TRUE)

      k_dist <- read.table(k_dist_file)
      retreat_prior_dens <- density(k_dist[,1], n = 10000, bw = 0.0703652)
      retreat_prior <- sample( retreat_prior_dens$x, 10000, replace = TRUE,
                               prob = retreat_prior_dens$y )

      #prior_min["retreat"] <- min(retreat_prior)
      #prior_max["retreat"] <- max(retreat_prior)
      prior_range[["retreat"]] <- range(retreat_prior)

      cat(sprintf("Empirical retreat prior range: [%.4f, %4.f]\n", min(retreat_prior), max(retreat_prior)), file = logfile_design, append = TRUE)

    }
  }

  # Other inputs ------------------------------------------------------------------------

  # USE RANGE FROM SIMULATIONS TO SET UNIFORM PRIORS
  for ( pp in ice_cont_list ) {

    # If not set yet
    if ( is.na(prior_range[[pp]][1])) {
      prior_range[[pp]][1] = min(ice_data[ , pp ])
      prior_range[[pp]][2] = max(ice_data[ , pp ])
    }
  }

  cat("\nIce model input ranges:\n",
      file = logfile_design, append = TRUE)
  for (pp in ice_cont_list ) {
    cat(sprintf( "%s: [%.1e, %.1e]", pp, prior_range[[pp]][1], prior_range[[pp]][2]),"\n",file = logfile_design, append = TRUE)
  }

  # Customise priors ------------------------------------------------------------------------

  # CUSTOMISE PRIORS FOR PROJECTIONS
  # N.B. this list doesn't include unif_temps because that design is for SA so should be wide and uniform
  if ( design_name %in% c("fixed_temp","AR6_2LM") && prior_choices == "custom" ) {

    # Ice sheets: restrict to higher resolution
    if (i_s %in% c("GIS", "AIS")) {

      # Maximum = 8km
      prior_range[["resolution"]][2] <- 8.0

    }

    # Greenland retreat empirical prior
    if ( i_s == "GIS" ) {

      # Cut 5% tails of empirical distribution off
      retreat_prior <- retreat_prior[ retreat_prior >= -0.9705 & retreat_prior <= 0.0070 ]

      # Get min and max of sample to use for main effects design
      prior_range[["retreat"]] <- range(retreat_prior)

      cat(sprintf("\nTRUNCATED RETREAT PRIOR TO 5-95%% RANGE: [%.4f, %4.f]\n",
                  min(retreat_prior),max(retreat_prior)), file = logfile_design, append = TRUE)
    }

    # Antarctica heat flux
    if ( i_s == "AIS" ) {

      # Halve the range
      prior_range[["heat_flux_ISMIP6_nonlocal"]][2] <-  prior_range[["heat_flux_ISMIP6_nonlocal"]][1] +
        0.5 * (prior_range[["heat_flux_ISMIP6_nonlocal"]][2] - prior_range[["heat_flux_ISMIP6_nonlocal"]][1])

      prior_range[["heat_flux_ISMIP6_nonlocal_slope"]][2] <-  prior_range[["heat_flux_ISMIP6_nonlocal_slope"]][1] +
        0.5 * (prior_range[["heat_flux_ISMIP6_nonlocal_slope"]][2] - prior_range[["heat_flux_ISMIP6_nonlocal_slope"]][1])

    }

    # Glaciers: restrict ranges
    if (i_s == "GLA" && "OGGM" %in% model_list ) {

      # Halve the max
      prior_range[["prec_corr_factor"]][2] <- prior_range[["prec_corr_factor"]][2] / 2.0

      # Halve the range, centred
      tb_range <- prior_range[["temp_bias"]][2] - prior_range[["temp_bias"]][1]
      prior_range[["temp_bias"]][1] <- prior_range[["temp_bias"]][1] + 0.25*tb_range
      prior_range[["temp_bias"]][2] <- prior_range[["temp_bias"]][2] - 0.25*tb_range

    }

    cat("\nFinal input ranges for projection priors:\n",
        file = logfile_design, append = TRUE)
    for (pp in ice_cont_list ) {
      cat(sprintf( "%s: [%.1e, %.1e]", pp, prior_range[[pp]][1], prior_range[[pp]][2]),"\n",file = logfile_design, append = TRUE)
    }

  }

  # ______________ -------------------------------------------------------------
  # Main effects ---------------------------------------------------------------
  # Design for each parameter
  # Vary one while others fixed

  # MAIN EFFECTS DESIGN: note not using SSPs, as want full range of GSAT-dependence
  # xxx Rather inefficient code...

  if (design_name == "main_effects") {

    nom <- list()
    oaat <- list()

    # Get fixed (nominal) and varying values for each column of design
    for ( dd in colnames(ice_design) ) { # or input_names

      # Climate column(s): mean
      if (dd %in% temps_list_names) {

        if (length(temps_list) == 1) { tt <- temps
        } else tt <- temps[ , which( temps_list_names == dd, arr.ind = TRUE)]

        nom[[ dd ]] <- mean(tt)
        oaat[[ dd ]] <- seq( from = min(tt), to = max(tt), length = N_samp )
      }

      # Continuous ice model inputs: mean unless special prior
      if (dd %in% ice_cont_list) {

        # Empirical prior
        if (i_s == "GIS" && dd == "retreat") {
          nom[[ dd ]] <- -0.17
        } else {
          # Uniform prior
          nom[[dd]] <- mean(prior_range[[dd]])
        }

        oaat[[ dd ]] <- seq( from = as.numeric(prior_range[[dd]][1]),
                             to = as.numeric(prior_range[[dd]][2]),
                             length = N_samp )

      }

      # Categorical ice model inputs (including dummy): 0
      if (dd %in% ice_dummy_list) {
        nom[[ dd ]] <- 0
        oaat[[ dd ]] <- 1
      }

      # Make matrix
      design_prior[[dd]] <- matrix(NA, nrow = length(oaat[[dd]]), ncol = length(input_names))
      colnames(design_prior[[dd]]) <- colnames(ice_design)  # input_names

      if (dd %in% ice_dummy_list) {

        # Add row for all dummy variables nominal i.e. reference value
        design_prior[[dd]] <- rbind(design_prior[[dd]], NA )

      }

    }


    # Fill with values
    for ( dd in colnames(ice_design) ) { # or input_names

      # Fill each column with nominal values
      for (cc in colnames(ice_design) ) {
        design_prior[[dd]][ , cc] <- nom[[cc]]
      }

      # Overwrite this column with OAAT
      # 1:length() is here because of extra nominal row for factor columns
      design_prior[[dd]][ 1:length(oaat[[dd]]), dd] <- oaat[[dd]]

    }




  } # design_name == "main_effects"

  #____________________________________________________________________________


  # Uniform GSAT ------------------------------------------------------------------------

  # Uniform in GSAT (for SSP)
  if (design_name == "unif_temps") {

    cat("\nPrior for GSAT: uniform over simulation data range\n",
        file = logfile_design, append = TRUE)

    # Create for each SSP
    for (scen in scenario_list) {

      design_prior_gsat <- matrix(nrow = N_samp, ncol = length(temps_list))
      colnames(design_prior_gsat) <- paste("GSAT_",temps_list)

      # GET MIN AND MAX OF SIMULATED CLIMATES
      for ( cc in 1:length(temps_list)) {

        if (length(temps_list) == 1) {
          min_temps = min(temps[ ice_data$scenario == scen ])
          max_temps = max(temps[ ice_data$scenario == scen ])
        } else {
          min_temps = min(temps[ ice_data$scenario == scen, cc])
          max_temps = max(temps[ ice_data$scenario == scen, cc])
        }

        # Uniform over range of original GCM forcings for that SSP
        design_prior_gsat[,cc] <- runif( N_samp, min = min_temps,
                                         max = max_temps)

        cat(sprintf( "%s %s: [%.1f, %.1f] degC\n", scen, temps_list_names[cc], min_temps, max_temps),
            file = logfile_design, append = TRUE)

      }

      # Create holder for all ice model input columns, including dummy variables
      design_prior_param <- matrix(nrow = N_samp, ncol = length(ice_all_list))
      colnames(design_prior_param) <- ice_all_list

      # Continuous parameters: sample independently
      for (pp in ice_cont_list) {

        if (i_s == "GIS" && pp == "retreat") {
          # Empirical
          samp <- sample( unlist(retreat_prior), N_samp, replace = TRUE )
        } else {
          # Uniform
          samp <- runif( N_samp, min = as.numeric(prior_range[[pp]][1]),
                         max = as.numeric(prior_range[[pp]][2]) )
        }

        # Store column
        design_prior_param[, pp] <- samp

      } # cont

      # Loop over factors (i.e. not dummy variables, because dept columns)
      for (pp in ice_factor_list) {

        if (is.na(pp)) next

        # Number of dummy variables = number of factor levels - 1 for collinearity
        nd <- length( ice_factor_values[[pp]] ) - 1

        # Sample from [1, 0, ...] for each row (trailing are more zeroes)
        prior_factor <- t(replicate( N_samp, sample( c(1,rep(0,nd)), nd, replace = FALSE )  ))

        # Reconstruct dummy variable column names (factor:level)
        # and check in dummy variable name list
        colnames_dv <- paste(pp, ice_factor_values[[pp]][-1], sep = ":")
        stopifnot( length( setdiff(colnames_dv, ice_dummy_list )) == 0 )

        # Store
        design_prior_param[ , colnames_dv ] <- prior_factor

      }

      # Combine climate and ice model priors
      design_prior[[scen]] <- cbind( design_prior_gsat, design_prior_param )
      colnames(design_prior[[scen]]) <- colnames(ice_design)

    } # scenarios

  } # unif_temps

  # Fixed GSAT ------------------------------------------------------------------------

  # Fixed GSAT value (multiple)
  if (design_name == "fixed_temp") {

    cat("\nPrior for GSAT: fixed value(s)\n",
        file = logfile_design, append = TRUE)
    fixed_temp_list <- c("mean_temp", "min_temp", "max_temp")

    # Create for each GSAT fixed value (call scen for consistency)
    for (scen in fixed_temp_list) {

      design_prior_gsat <- matrix(nrow = N_samp, ncol = length(temps_list))
      colnames(design_prior_gsat) <- paste("GSAT_",temps_list)

      # GET MIN AND MAX OF SIMULATED CLIMATES
      for ( cc in 1:length(temps_list)) {

        if (length(temps_list) == 1) { tt <- temps
        } else tt <- temps[ , cc]

        if (scen == "mean_temp") fixed_temp <- mean(tt)
        if (scen == "min_temp") fixed_temp <- min(tt)
        if (scen == "max_temp") fixed_temp <- max(tt)

        # Just use this value
        design_prior_gsat[,cc] <- rep( fixed_temp, N_samp )

        cat(sprintf( "%s %s: %.1f degC\n", scen, temps_list_names[cc], fixed_temp),
            file = logfile_design, append = TRUE)

      }

      # Rest of this is duplicated from unif_temp code

      # Create holder for all ice model input columns, including dummy variables
      design_prior_param <- matrix(nrow = N_samp, ncol = length(ice_all_list))
      colnames(design_prior_param) <- ice_all_list

      # Continuous parameters: sample independently
      for (pp in ice_cont_list) {

        if (i_s == "GIS" && pp == "retreat") {
          # Empirical
          samp <- sample( unlist(retreat_prior), N_samp, replace = TRUE )
        } else {
          # Uniform
          samp <- runif( N_samp, min = as.numeric(prior_range[[pp]][1]),
                         max = as.numeric(prior_range[[pp]][2]) )
        }

        # Store column
        design_prior_param[, pp] <- samp

      } # cont

      # Loop over factors (i.e. not dummy variables, because dept columns)
      for (pp in ice_factor_list) {

        if (is.na(pp)) next

        # Number of dummy variables = number of factor levels - 1 for collinearity
        nd <- length( ice_factor_values[[pp]] ) - 1

        # Sample from [1, 0, ...] for each row (trailing are more zeroes)
        prior_factor <- t(replicate( N_samp, sample( c(1,rep(0,nd)), nd, replace = FALSE )  ))

        # Reconstruct dummy variable column names (factor:level)
        # and check in dummy variable name list
        colnames_dv <- paste(pp, ice_factor_values[[pp]][-1], sep = ":")
        stopifnot( length( setdiff(colnames_dv, ice_dummy_list )) == 0 )

        # Store
        design_prior_param[ , colnames_dv ] <- prior_factor

      }

      # Combine climate and ice model priors
      design_prior[[scen]] <- cbind( design_prior_gsat, design_prior_param )
      colnames(design_prior[[scen]]) <- colnames(ice_design)

    } # fixed_temp_list

  } # fixed_temp


  # AR6 prior ------------------------------------------------------------------------


  # COMBINE WITH CLIMATE PRIOR: AR6
  # set up alternative or stopifnot
  if (design_name == "AR6_2LM") {

    cat("\nPrior for GSAT: AR6 two-layer model ensemble\n", file = logfile_design, append = TRUE)

    # Read simple climate model CSV file specified in main.R
    # cat(paste("Reading CSV file of GSAT projections:", climate_data_file, "\n"), file = logfile_design, append = TRUE)
    # climate_prior_all <- read.csv(paste0(inputs_preprocess, "GSAT/", climate_data_file))

    # Read simple climate model netcdf file specified as an arg
    cat(paste("Reading netcdf file of GSAT projections:", climate_data_file, "\n"), file = logfile_design, append = TRUE)

    # Open file and read into data frame
    ncin <- ncdf4::nc_open( climate_data_file )
    climate_prior_all <- as.data.frame(ncdf4::ncvar_get(ncin,paste0(facts_ssp,"/surface_temperature")))

    # Add years as column names
    year <- ncdf4::ncvar_get(ncin,"year")
    colnames(climate_prior_all) <- paste0("y", year)

    # Design for each SSP
    for (scen in scenario_list) {

      # GET CLIMATE PRIOR

      # Get all FaIR 2LM projections for this scenario
      # climate_prior <- climate_prior_all[ climate_prior_all$scenario == scen, ] # only needed when reading CSV
      climate_prior <- climate_prior_all

      # Create matrix for GSAT values
      if (length(temps_list) == 1) {
        design_prior_gsat <- rep(NA, dim(climate_prior)[1])
      } else {
        design_prior_gsat <- matrix( NA, nrow = dim(climate_prior)[1], ncol = length(temps_list) )
        colnames(design_prior_gsat) <- paste0("y", temps_list)
      }

      # Years for baseline
      temps_period1 <- temps_baseline + 1:N_temp_yrs - 1

      cat( paste("GSAT prior: baseline mean period", paste(range(temps_period1), collapse = "-"), "\n"),
           file = logfile_design, append = TRUE )

      # Calculate decadal means and subtract baseline
      for (ss in 1:dim(climate_prior)[1]) {

        # Get timeslice(s) needed for prediction
        if (length(temps_list) == 1) {

          temps_period2 <- temps_list - N_temp_yrs:1 + 1

          if (ss == 1) cat( paste("GSAT prior: forcing mean period", paste(range(temps_period2), collapse = "-"), "\n"),
                            file = logfile_design, append = TRUE )
          design_prior_gsat[ ss ] <- mean(unlist(climate_prior[ ss, paste0("y", temps_period2) ])) - mean(unlist(climate_prior[ ss, paste0("y", temps_period1) ]))

        } else {

          for ( tt in temps_list ) {

            temps_period2 <- tt - N_temp_yrs:1 + 1

            if (ss == 1) cat( paste("GSAT prior: forcing mean period", paste(range(temps_period2), collapse = "-"), "\n"),
                              file = logfile_design, append = TRUE )

            design_prior_gsat[ ss, paste0("y", tt) ] <- mean(unlist(climate_prior[ ss, paste0("y", temps_period2) ])) - mean(unlist(climate_prior[ ss, paste0("y", temps_period1) ]))

          }
        }
      }

      if (length(temps_list) == 1) { N_temp <- length(design_prior_gsat)
      } else N_temp <- dim(design_prior_gsat)[1]

      cat(paste("Got", N_temp, "GSAT samples from file for prior\n"), file = logfile_design, append = TRUE)

      # Dataset check: number of 2LM projections for each SSP
      if (length(temps_list) > 1) stopifnot( dim(design_prior_gsat)[2] == length(temps_list) )

      # COMBINE WITH ICE SHEET MODEL PARAMETER PRIOR __________

      # Get N_temp samples from parameter priors
      # Resamples for each SSP (unlike emulandice v1)

      # Create holder for all ice model input columns, including dummy variables
      design_prior_param <- matrix(nrow = N_temp, ncol = length(ice_all_list))
      colnames(design_prior_param) <- ice_all_list

      # Continuous parameters
      for (pp in ice_cont_list) {

        if (i_s == "GIS" && pp == "retreat") {
          # Empirical
          samp <- sample( unlist(retreat_prior), N_temp, replace = TRUE )
        } else {
          # Uniform
          samp <- runif( N_temp, min = as.numeric(prior_range[[pp]][1]),
                         max = as.numeric(prior_range[[pp]][2]) )
        }

        # Store column
        design_prior_param[, pp] <- samp

      }

      # Loop over factors (i.e. not dummy variables, because dept columns)
      for (pp in ice_factor_list) {

        if (is.na(pp)) next

        # Number of dummy variables = number of factor levels - 1 for collinearity
        nd <- length( ice_factor_values[[pp]] ) - 1

        # Sample from [1, 0, ...] for each row (trailing are more zeroes)
        prior_factor <- t(replicate( N_temp, sample( c(1,rep(0,nd)), nd, replace = FALSE )  ))

        # Reconstruct dummy variable column names (factor:level)
        # and check in dummy variable name list
        colnames_dv <- paste(pp, ice_factor_values[[pp]][-1], sep = ":")
        stopifnot( length( setdiff(colnames_dv, ice_dummy_list )) == 0 )

        # Store
        design_prior_param[ , colnames_dv ] <- prior_factor

      }

      # Merge into one design
      design_prior[[scen]] <- cbind( design_prior_gsat, design_prior_param )
      colnames(design_prior[[scen]]) <- colnames(ice_design)

    } # scenario_list

    # Print out and keep this N_temp value for writing CSV
    cat(paste("Loaded",N_temp,"samples for each scenario\n"), file = logfile_design, append = TRUE)

  } # AR6_2LM design

  # Return design  ------------------------------------------------------------------------

  cat(paste("\nload_design_pred: created prior designs for",paste(names(design_prior), collapse = " "),"\n"), file = logfile_design, append = TRUE)

  return(design_prior)

} # end of function


