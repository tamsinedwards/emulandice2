#' write_outputs: write CSVs of projections.
#'
#' @description Write projections to CSV: just final (i.e. with emulator
#' uncertainties), or optionally also the mean.
#'
#' @param write_mean TRUE/FALSE to write mean projections, as well as the
#' final/full projections with emulator uncertainties.
#'
#' @export

# CSV output files: nrows = GSAT samples (2237 for AR6) x years_em timeslices
# Netcdf output files: same dimensions but also 1 'location'

# XXX Writing zero in cal_start year column - add this to projections instead!

write_outputs <- function(write_mean) {

  # XXX Should write parameter values to a separate file for reference


  cat("\n_____________________________________\n",file = logfile_results, append = TRUE)
  cat("write_outputs: writing CSV files...\n", file = logfile_results, append = TRUE)

  # Two CSVs for each scenario: with and without emulator uncertainty
  # xxx Structure needs thinking about (mainly in write_output.R)
  # e.g. just output land ice contribution for each experiment for each SSP
  # and write any other parameters in another file
  if (write_mean) csv_full_mean <- list()
  csv_full_final <- list()

  for (scen in scenario_list) {

    csv_header <- paste0("ice_source,region,sample,",paste("SLE", c(cal_start, years_em), sep = "_", collapse = ","),"\n")

    if (write_mean) {
      csv_full_mean[[scen]] <- paste0( outdir, out_name,"_projections_MEAN_",
                                       scen, ".csv")
      cat( csv_header, file = csv_full_mean[[scen]] )
    }

    csv_full_final[[scen]] <- paste0( outdir, out_name,"_projections_FULL_",
                                      scen, ".csv")
    cat( csv_header, file = csv_full_final[[scen]] )

  }

  if (write_mean) {

    cat("Mean projections\n", file = logfile_results, append = TRUE)
    # No emulator uncertainty
    for (scen in scenario_list) {
        for (tt in 1:N_temp) {
          #cat( sprintf("%s,%s,%i,%i,%.4f\n",# %.4f,%.4f, %i
          #             i_s, reg, yy, # Ice source, region, year
          #             tt,
          #             myem[[scen]]$mean[, paste0("y",yy) ][tt] ),
           #    file = csv_full_mean[[scen]], append = TRUE )

          cat( sprintf("%s,%s,%i,%.1f, %s\n",
                       i_s, reg, tt, 0.0,
                       paste(myem[[scen]]$mean[ tt, paste0("y",years_em) ], collapse = ",")),
               file = csv_full_mean[[scen]], append = TRUE )
        }

    }
  }

  cat("Full projections\n", file = logfile_results, append = TRUE)

  # With emulator uncertainty
  for (scen in scenario_list) {
      for (tt in 1:N_temp) {
        cat( sprintf("%s,%s,%i,%.1f, %s\n",
                     i_s, reg, tt, 0.0,
                     paste(projections[[scen]][ tt, paste0("y",years_em) ], collapse = ",")),
             file = csv_full_final[[scen]], append = TRUE )
      }
   # }

    # xxx CHANGE TO FILLING AN ARRAY ONCE!
    csv_full <- read.csv(csv_full_final[[scen]])
    csv_mat <- 10.0* t( csv_full[ , paste0("SLE_",years_em) ] ) # cm to mm

    # Backwards hack FACTS type name
    print(facts_ssp)
    baseyear <- paste0(cal_start,"LL")
    ncname <- paste0(outdir, "emulandice.",facts_ssp,".emu",i_s,".emulandice.",i_s,"_",reg,"_globalsl.nc")

    # Define dimensions
    timedim <- ncdf4::ncdim_def("years","years",as.integer(years_em))
    sampledim <- ncdf4::ncdim_def("samples","samples",0:(N_temp-1))
    locdim <- ncdf4::ncdim_def("locations","locations",as.integer(-1))
    slc_def <- ncdf4::ncvar_def("sea_level_change","mm",list(sampledim, timedim, locdim),prec="short")

    # Create file
    ncout <- ncdf4::nc_create( ncname, list(slc_def), force_v4 = TRUE )
    print(paste("Writing",ncname))

    # Fill with an array; 1 location
    csv_data <- array(NA, dim = c(length(years_em), N_temp, 1))
    csv_data[ , ,1] <- as.matrix( csv_mat )
    ncdf4::ncvar_put(ncout,slc_def,csv_data)
    ncdf4::ncatt_put(ncout,0,"description",paste("Global SLR contribution from",ice_name,"using the emulandice2 module"))
    ncdf4::ncatt_put(ncout,0,"history",paste("Created", date()))
    ncdf4::ncatt_put(ncout,0,"source",paste0("FACTS: emulandice.",facts_ssp,".emu",i_s,".emulandice.",reg))
    ncdf4::ncatt_put(ncout,0,"baseyear", baseyear)
    ncdf4::ncatt_put(ncout,0,"scenario",facts_ssp)
    ncdf4::nc_close(ncout)
  }

  cat("\n_____________________________________\n",file = logfile_results, append = TRUE)

}
