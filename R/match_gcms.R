#' match_gcms: match ice simulations to climate forcing simulations
#'
#' @description
#' Look up climate simulation for each ice simulation by SSP and GCM, check
#' it exists, and calculate climate change for emulator input.
#'
#' @returns `match_gcms()` returns requested temperature change(s) for each
#' simulation.
#'
#' @export

match_gcms <- function(ice_data, temps_dataset) {

  # Match climate and ice sims ---------------------------------------------------------------------

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat("match_gcms: matching ice model simulations with forcing simulations\n\n",
      file = logfile_build, append = TRUE)

  # Write temps into ice_data length matrix, not forcing length, to use in ice_design
  temps <- ice_data[ , c("scenario", "GCM")]

  # For each GSAT
  for (tt in 1:length(temps_list)) {

    temps <- cbind(temps, NA)
    colnames(temps)[ tt + 2 ] <- paste0("GSAT_", temps_list[tt])

    # Fill column
    temps[ , tt + 2] <- unlist(apply(ice_data, 1, function(x) {

      # For each row in ice_data (simulation), get temps_dataset
      temp_row <- temps_dataset[ temps_dataset$GCM == x[ "GCM" ]
                                & temps_dataset$scenario == x[ "scenario"], tt + 2 ]
      if (length(temp_row) > 0) { temp_row
      } else NA
    }))

  }

  # Useful to know what we have: rows with data in final column
  df_found <- unique( temps[ ! is.na(temps[ , 2 + length(temps_list)]),] )
  cat(paste("Found", dim(df_found)[1], "forcings for",
            dim(temps[ ! is.na(temps[ , 2 + length(temps_list)]),])[1], "simulations:\n"),
      file = logfile_build, append = TRUE)
  cat(paste(df_found[ ,"scenario"], df_found[ ,"GCM"], "\n"), "\n",
      file = logfile_build, append = TRUE)

  # And what we don't: missing final column
  df_miss <- unique( temps[is.na(temps[ , 2 + length(temps_list)]),] )
  cat(paste("Could not find part/all of", dim(df_miss)[1],"forcings for", dim(temps[is.na(temps[ , 2 + length(temps_list)]),])[1],
            "simulations so skipped these:\n"), file = logfile_build, append = TRUE)
  cat(paste(df_miss[ ,"scenario"], df_miss[ ,"GCM"], "\n"), "\n", file = logfile_build, append = TRUE)


  cat(paste("\nmatch_gcms: found", dim(temps[ ! is.na(temps[ , 2 + length(temps_list)]),])[1], "of", dim(temps)[1],
            "forcing simulations\n"), file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  return(temps)

}
