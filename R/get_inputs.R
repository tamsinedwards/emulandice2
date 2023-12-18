#' get_inputs: get input values for emulation
#'
#' @description
#' Get input values for emulation and impute missing values.
#' XXX Impute all / factors etc in CSV instead?
#'
#' @param param_list Requested ice model inputs (continuous and categorical).
#'
#' @returns `get_inputs()` returns requested (and imputed) columns from ice
#' data matrix.
#'
#' @export

get_inputs <- function(param_list) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("get_inputs: get ice model inputs\n",file = logfile_build, append = TRUE)

  cat("\nRequested inputs:\n",file = logfile_build, append = TRUE)
  cat( paste(c(param_list, "\n")), file = logfile_build, append = TRUE)

  # xxx Probably need to adapt this for AR6 glaciers? (as no ice_param)
  ice_param <- ice_data[ param_list ]

  #==========================================================================
  # RENAME INPUTS

  # AR6: rename GIS melt to proper parameter name
#  if (dataset == "IPCC_AR6" && i_s == "GrIS") names(ice_param) <- "retreat"

  #==========================================================================
  # IMPUTE MISSING VALUES
  # XXX CHANGE THIS TO GENERIC IMPUTING USING JONTY RECOMMENDED METHOD

#  if (dataset == "PROTECT") {

    # Glacier forcing ensemble has NAs for all model inputs
    if (i_s == "GLA") {

      # forcing ensemble have NAs for all params
      # multi-model emulator has NAs because not imputed in CSV -> XXX NEED TO DO THIS
      ice_param_ind <- is.na(ice_param)
      ice_param_miss <- length(ice_param[ ice_param_ind ] )

      if (ice_param_miss > 0) {

        ice_param_mean <- 0

        cat(paste0("Imputing ",ice_param_miss,
                     " missing values for ", param_list,
                     " with normal dist with mean & sd (",ice_param_mean, ", 1)\n"),
                   file = logfile_build, append = TRUE)
        ice_param[ ice_param_ind, 1] <-
          rnorm( length(ice_param[ is.na(ice_param), 1]), mean = ice_param_mean,
                 sd = 1 )
      }
    }

#  }

  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  return( ice_param )

}
