#' Calculate ice sheet regional SLE fractions and (optionally) plot histograms
#'
#' @description
#' GIS: mean basin / ALL over selected rows.
#' AIS: median subregion / `{reg}` then renormalise to sum 1.
#' Uses raw m SLE CSVs (same date as load_sims), not ice_data year columns.
#'
#' @returns list(region_names, region_fracs)
#' @export

# General helper ---------------------------------------

calc_region_fracs <- function() {

  stopifnot(i_s %in% c("GIS", "AIS"), do_regions)

  cat("\nIce sheet regional fraction calculation\n\n",
      file = logfile_build, append = TRUE)

  # Get row numbers i.e. selected simulations of main dataset
  sims_index <- rownames(ice_data[, paste0("y", years_em)])
  stopifnot(length(sims_index) == nrow(ice_data), length(sims_index) > 0)

  if (i_s == "GIS") out <- calc_gis_region_fracs(sims_index)
  if (i_s == "AIS") out <- calc_ais_region_fracs(sims_index)

  out
}

## GIS fractions ---------------------------------------
calc_gis_region_fracs <- function(sims_index) {

  stopifnot(i_s == "GIS")

  # Translate CSV regions to nicer names for netcdf files
  region_names <- list()
  region_names[["nw"]] <- "NW"
  region_names[["no"]] <- "NO"
  region_names[["cw"]] <- "CW"
  region_names[["ne"]] <- "NE"
  region_names[["sw"]] <- "SW"
  region_names[["se"]] <- "SE"

  # This file has ALL + 6 regions
  region_file <- read.csv(paste0( inputs_preprocess, "/GIS/GIS_SIMULATIONS_m_SLE_2014_260819.csv"))

  # All simulations (to construct index)
  all <- region_file[ region_file$region == "ALL",  ]
  nrows_all <- nrow(all)

  # Timeslices for sims selected in main analysis
  all <- all[ sims_index, paste0("y", years_em) ]

  # Open plot file for histograms
  if (plot_level > 1) {
    pdf( file = paste0( plotdir, out_name, "_region_fractions.pdf" ))
    par(mfrow = c(3,2)) # 6 panels per page
  }

  region_fracs_all <- list() # for histograms for each region
  region_fracs <- list() # mean or adjusted median fraction for each region

  for (rr in names(region_names) ) {

    # Get all simulations for region and number rows
    region_all <- region_file[ region_file$region == rr, paste0("y", years_em) ]
    rownames(region_all) <- 1:nrows_all

    rr_name <- region_names[[rr]]

    # Pick same rows as main analysis
    region_all <- region_all[ sims_index, ]

    # Calculate fractions (all timeslices in all simulations)
    region_fracs_all <- unlist(  region_all / all )

    # Mean of these
    region_fracs[[ rr_name ]] <- mean(region_fracs_all, na.rm = TRUE)

    # Print to file
    cat( sprintf( "%s: %.4f\n", rr_name,
                  region_fracs[[ rr_name ]] ), file = logfile_build, append = TRUE)

    # Plot
    if (plot_level > 1) {

      hist(region_fracs_all, xlim = c(0,1),
           breaks = seq(from = floor(min(region_fracs_all, na.rm = TRUE)),
                        to = ceiling(max(region_fracs_all, na.rm = TRUE)), by = 0.01),
           main = paste0(ice_name, ": ", rr_name), xlab = "Fraction" )
      abline(v = region_fracs[[ rr_name ]], lwd = 2, col = "blue")
      text( 0.7, 300, sprintf("Mean: %.3f",
                              region_fracs[[ rr_name ]]), col = "blue")
    }


  }

  if (plot_level > 0) dev.off()

  tot_adj <- sum(unlist(region_fracs))

  cat( paste("\nTotal:",  tot_adj, "\n\n"), file = logfile_build, append = TRUE)
  stopifnot(abs(tot_adj - 1.0) < 1e-5)

  list(region_names = region_names, region_fracs = region_fracs)

}

# AIS fractions ---------------------------------------
calc_ais_region_fracs <- function(sims_index) {

  stopifnot(i_s == "AIS")

  # ASE, Ross, RF; Peninsula; East
  ais_all_names <- c("WAIS1", "WAIS2", "WAIS3", "PEN", paste0("EAIS", 1:7))

  region_names <- list()
  if (reg == "ALL") region_names <- ais_all_names
  if (reg == "WAIS") region_names <- c( "WAIS1", "WAIS2", "WAIS3")
  if (reg == "PEN") region_names <- c("PEN")
  if (reg == "EAIS") region_names <- paste0("EAIS", 1:7)

  # All simulations (to construct index)
  all <- read.csv(paste0(inputs_preprocess,
                         "/AIS/AIS_SIMULATIONS_", reg, "_m_SLE_2014_260819.csv"))

  # Save nrows before indexing
  nrows_all <- nrow(all)

  # Timeslices for sims selected in main analysis
  all <- all[ sims_index, paste0("y", years_em) ]

  region_fracs_all <- list() # for histograms for each region
  region_fracs <- list() # mean or adjusted median fraction for each region

  cat("Before normalisation:\n", file = logfile_build, append = TRUE)

  for (rr_name in region_names) {

    rr <- match(rr_name, ais_all_names)
    stopifnot(!is.na(rr))

    region_file <- read.csv(
      sprintf("%s/AIS/regions/AIS_SIMULATIONS_region_%i_m_SLE_2014_260819.csv",
              inputs_preprocess, rr))
    stopifnot(nrow(region_file) == nrows_all)
    stopifnot(all(region_file$region == rr_name))

    # Get all simulations for region and number rows
    region_all <- region_file[ , paste0("y", years_em) ]
    rownames(region_all) <- 1:nrows_all

    # Pick same rows as main analysis
    region_all <- region_all[ sims_index, ]

    # Calculate fractions (all timeslices in all simulations)
    region_fracs_all[[ rr_name ]] <- as.numeric(unlist(region_all))  / as.numeric(unlist(all))

    # Replace infinities with missing
    region_fracs_all[[ rr_name ]][is.infinite(region_fracs_all[[ rr_name ]])] <- NA

    # Calculate MEDIAN not mean for Antarctica
    region_fracs[[ rr_name ]] <- median(region_fracs_all[[ rr_name ]], na.rm = TRUE)

    # Print to file
    cat( sprintf( "%s: %.4f\n", rr_name,
                  region_fracs[[ rr_name ]] ), file = logfile_build, append = TRUE)

  }

  total_median <- sum(unlist(region_fracs))

  cat( paste("\nTotal of medians (not expected to be 1.0):", total_median, "\n"), file = logfile_build, append = TRUE)

  missing <- 1.0 - total_median
  cat(sprintf("\nMissing fraction before adjustment: %.3f\n", missing), file = logfile_build, append = TRUE)

  # Get median fractions to adjust
  region_fracs_adj <- unlist(region_fracs)

  # Sort sectors - this is from when I redistributed only to largest sectors
  # but keep for interest
  cat("\nRegions in decreasing order of contribution:\n", file = logfile_build, append = TRUE)
  sec_sort <- sort(unlist(region_fracs_adj), decreasing = T, index.return = T)
  for (ss in sec_sort$ix) {
    cat(sprintf("%s: %.1f%%\n", region_names[ss], 100.0*region_fracs_adj[ss]), file = logfile_build, append = TRUE)
  }

  # Pdf later than for GIS because adjusting fractions
  if (plot_level > 0 && reg != "PEN") {
    pdf( file = paste0( plotdir, out_name, "_region_fractions.pdf" ))
    if (reg %in% c("ALL","EAIS")) par(mfrow = c(3,2)) # 6 panels per page for 11 or 7 regions
  }

  cat("\nAdjust median fractions to sum to 1:\n", file = logfile_build, append = TRUE)

  region_fracs_adj <- region_fracs_adj / (sum(region_fracs_adj))

  for (ss in 1:length(region_names)) {

    miss_bits <- region_fracs_adj[ss] - region_fracs[[ss]]

    cat( sprintf("%s: median = %.3f, adjusted = %.3f (%.0f%% adjustment)\n",
                 region_names[ss], region_fracs[[ss]], region_fracs_adj[ss], 100.0*miss_bits / region_fracs[[ss]] ),
         file = logfile_build, append = TRUE)

    # Plot histograms now so can show median and adjusted together
    # no need for PEN because it is not subdivided: fraction = 1.0
    if (plot_level > 0 && reg != "PEN") {

      hist_xmin <- -2
      hist_xmax <- 2
      hist_breaks <- c( floor(min(region_fracs_all[[ ss ]], na.rm = TRUE)),
                        seq(from = hist_xmin, to = hist_xmax, by = 0.01),
                        ceiling(max(region_fracs_all[[ ss ]], na.rm = TRUE)) )

      get_y <- hist(region_fracs_all[[ ss ]], breaks = hist_breaks, plot = FALSE )

      hist(region_fracs_all[[ ss ]], xlim = c(hist_xmin,hist_xmax), breaks = hist_breaks,
           main = paste0(ice_name, ": ", region_names[[ss]]), xlab = "Fraction" )

      # Lines and labels
      abline(v = region_fracs[[ ss ]], lwd = 2, col = "darkred", lty = 3)
      abline(v = region_fracs_adj[ ss ], lwd = 2, col = "red", lty = 2)
      text( hist_xmin + 0.2, 0.9*max(get_y$density), pos = 4,
            sprintf("Median: %.3f", region_fracs[[ ss ]]), col = "darkred", cex = 0.9)
      text( hist_xmin + 0.2, 0.8*max(get_y$density), pos = 4,
            sprintf("Adjusted: %.3f", region_fracs_adj[ ss ]), col = "red", cex = 0.9)
    }


    # Overwrite original fraction list with adjusted AFTER plotting histograms
    region_fracs[[ss]] <- region_fracs_adj[ss]

  }

  if (plot_level > 0 && reg != "PEN") dev.off()

  tot_adj <- sum(unlist(region_fracs))

  cat(sprintf("Fractions after adjustment (should be 1.0) = %.3f\n", tot_adj), file = logfile_build, append = TRUE)
  stopifnot(abs(tot_adj - 1.0) < 1e-4)

  list(region_names = region_names, region_fracs = region_fracs)

}

