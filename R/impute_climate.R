#' impute_climate: fill missing data in climate forcings
#'
#' @description
#' Imputes missing 2100 and 2300 values if 2099 and 2299 values are there
#' Constructs fixed climate for GIS post-2100 fixed_climate = 2100 simulations
#' Drops columns that are not needed
#'
#' @param climate_dataset Dataset
#' @param construct_fixed Set to TRUE to construct post-2100 forcings for GIS
#'
#' @returns `impute_climate()` returns climate dataset
#'
#' @export

impute_climate <- function(climate_dataset, construct_fixed = FALSE) {

  cat("\n_____________________________________\n",file = logfile_build, append = TRUE)
  cat(paste("impute_climate: fill missing 2100 and 2300\n\n"),
      file = logfile_build, append = TRUE)

  # Impute two common missing cases that mean a simulation would be dropped unnecssarily

  # GCM only reached 2099: impute 2100 with this value
  miss_ind <- is.na(climate_dataset$y2100) & !is.na(climate_dataset$y2099)
  if (length(miss_ind[miss_ind]) > 0) {
    cat(sprintf("Imputing %i GCM simulations by setting 2100 to 2099 value:\n", length(miss_ind[miss_ind])),
        file = logfile_build, append = TRUE)
    cat(paste(climate_dataset[ miss_ind, c("scenario")], climate_dataset[ miss_ind, c("GCM")], "\n"), "\n",
        file = logfile_build, append = TRUE)
    climate_dataset[ miss_ind, "y2100"] <- climate_dataset[ miss_ind, "y2099"]
  }

  # Repeat for 2299/2300
  miss_ind <- is.na(climate_dataset$y2300) & !is.na(climate_dataset$y2299)
  if (length(miss_ind[miss_ind]) > 0) {
    cat(sprintf("Imputing %i GCM simulations by setting 2300 to 2299 value:\n", length(miss_ind[miss_ind])),
        file = logfile_build, append = TRUE)
    cat(paste(climate_dataset[ miss_ind, c("scenario")], climate_dataset[ miss_ind, c("GCM")], "\n"), "\n",
        file = logfile_build, append = TRUE)
    climate_dataset[ miss_ind, "y2300"] <- climate_dataset[ miss_ind, "y2299"]
  }


  # Construct whole duplicate array of forcings with fixed climate from 2100
  # Not very efficient, but very many are used in ensemble and saves index errors too
  if (construct_fixed) {

    cat(paste("Reconstructing fixed climates from 2100\n"),
        file = logfile_build, append = TRUE)

    # Index for each decade after fixed date
    decadal_ind <- seq(from = 2101, to = 2291, by = 10)

    # Paste 2091-2100 values into these
    for (dd in 1:length(decadal_ind)) {
      climate_dataset[ , paste0("y", decadal_ind[dd] + 0:9)] <- climate_dataset[ , paste0("y", 2091:2100)]
    }

  }

  # Only need scenario, GCM, and date range of simulations
  return( climate_dataset[, c("scenario", "GCM", paste0("y", first_year:final_year)) ] )

}
