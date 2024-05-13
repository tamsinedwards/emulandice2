#' write_outputs: write CSVs of projections.
#'
#' @description Write projections to CSV: just final (i.e. with emulator
#' uncertainties), or optionally also the mean.
#'
#' @param write_csv TRUE/FALSE to write projections to CSV
#'
#' @export

# CSV output files: nrows = GSAT samples x years_em timeslices
# Netcdf output files: same dimensions but also 1 'location'

# XXX Writing zero in cal_start year column - add this to projections instead!

write_outputs <- function(write_mean) {

  # XXX Should write parameter values to a separate file for reference

  if (write_csv) {

    cat("\n_____________________________________\n",file = logfile_results, append = TRUE)
    cat("write_outputs: writing CSV files\n", file = logfile_results, append = TRUE)

    # CSVs for scenario:
    # with (mean) and without (final) emulator uncertainty
    # final with history matching or Bayesian calibration

    csv_mean <- list()
    csv_final <- list()
    csv_nroy <- list()
    csv_post <- list()

    for (scen in scenario_list) {

      # Write header to each file
      csv_header <- paste0("ice_source,region,sample,",
                           paste("SLE", c(cal_start, years_em), sep = "_", collapse = ","),"\n")

      csv_mean[[scen]] <- paste0( outdir_facts, out_name,"_projections_MEAN_",scen, ".csv")
      cat( csv_header, file = csv_mean[[scen]] )

      csv_final[[scen]] <- paste0( outdir_facts, out_name,"_projections_FULL_",scen, ".csv")
      cat( csv_header, file = csv_final[[scen]] )

      csv_nroy[[scen]] <- paste0( outdir_facts, out_name,"_projections_NROY_",scen, ".csv")
      cat( csv_header, file = csv_nroy[[scen]] )

      csv_post[[scen]] <- paste0( outdir_facts, out_name,"_projections_POSTERIOR_",scen, ".csv")
      cat( csv_header, file = csv_post[[scen]] )

      # No emulator uncertainty
      for (tt in 1:N_temp) {
        cat( sprintf("%s,%s,%i,%.1f, %s\n",
                     i_s, reg, tt, 0.0,
                     paste(myem[[scen]]$mean[ tt, paste0("y",years_em) ], collapse = ",")),
             file = csv_mean[[scen]], append = TRUE )
      }

      # With emulator uncertainty
      for (tt in 1:N_temp) {
        cat( sprintf("%s,%s,%i,%.1f, %s\n",i_s, reg, tt, 0.0,
                     paste(projections[[scen]][ tt, paste0("y",years_em) ], collapse = ",")),
             file = csv_final[[scen]], append = TRUE )
      }

      # History matched: note will not be N_temp length
      # Values should exactly match those with same row index (tt) in FULL csv above
      # Could have added flag in column in that file, but this keeps header consistent across all files
      for (tt in 1:N_temp) {
        if (tt %in% proj_nroy[[scen]]) {
          cat( sprintf("%s,%s,%i,%.1f, %s\n",i_s, reg, tt, 0.0,
                       paste(projections[[scen]][ tt, paste0("y",years_em) ], collapse = ",")),
               file = csv_nroy[[scen]], append = TRUE )
        }
      }

      # Bayesian calibration: resampled from full projections
      for (tt in 1:N_temp) {
        cat( sprintf("%s,%s,%i,%.1f, %s\n", i_s, reg, tt, 0.0,
                     paste(proj_post[[scen]][ tt, paste0("y",years_em) ], collapse = ",")),
             file = csv_post[[scen]], append = TRUE )
      }


    } # scenario list

  } # write CSV


  # SETUP NETCDF STRUCTURE FIRST

  # xxx Currently fixed baseline, but this will change
  baseyear <- paste0(cal_start,"LL")

  # Define dimensions
  timedim <- ncdf4::ncdim_def("years","",longname="",as.integer(years_em))
  sampledim <- ncdf4::ncdim_def("samples","",longname="",0:(N_temp-1))
  locdim <- ncdf4::ncdim_def("locations","",longname="",as.integer(-1))

  # Define variables
  slc_def <- ncdf4::ncvar_def("sea_level_change","mm",list(timedim, sampledim, locdim),prec="short")
  lat_def <- ncdf4::ncvar_def("lat","",list(locdim),prec="float",missval=NA)
  lon_def <- ncdf4::ncvar_def("lon","",list(locdim),prec="float",missval=NA)


  # REGION(S) TO WRITE
  regions_write <- reg

  # Loop for writing ice sheet regional files (and later glacer regions, if modelled jointly)
  if (i_s %in% c("GIS", "AIS")) regions_write <- c(regions_write, region_names)


  # VARIABLE TO WRITE: transpose posterior projections and convert cm to mm for FACTS
  var_to_write <- as.matrix( 10.0* t( proj_post[[scen]] ) )

  for ( rr in regions_write ) {

    if (rr == reg) { ncname_reg <- ncname
    } else {
      # Use globalsl extension to avoid accidental extra substitutions
      ncname_reg <- gsub( paste(reg, "globalsl", sep = "_"), paste(rr, "globalsl", sep = "_"), ncname )
    }

    cat("write_outputs: writing netcdf file ", ncname_reg, "\n", file = logfile_results, append = TRUE)
    print(paste("Writing",ncname_reg))

    # Create file
    ncout <- ncdf4::nc_create( ncname_reg, list(slc_def, lat_def, lon_def), force_v4 = TRUE )

    # Fill with an array; 1 location
    csv_data <- array(NA, dim = c(length(years_em), N_temp, 1))

    # SL contribution for full projection or ice sheet regional fraction
    if (rr == reg) { csv_data[ , ,1] <- var_to_write
    } else {
      # Fixed fraction of total
      csv_data[ , ,1] <- var_to_write * region_fracs[[ rr ]]
    }

    ncdf4::ncvar_put(ncout,slc_def,csv_data)

    # Add for consistency with FACTS formats
    ncdf4::ncvar_put(ncout,lat_def, NA)
    ncdf4::ncvar_put(ncout,lon_def, NA)

    # Attributes
    if (rr == reg) { ncdf4::ncatt_put( ncout,0,"description", paste0("Global SLR contribution from ", ice_name, " using the emulandice2 module"))
    } else ncdf4::ncatt_put( ncout,0,"description", paste("Global SLR contribution from", ice_name, "(",rr,")", "using the emulandice2 module") )

    ncdf4::ncatt_put(ncout,0,"history",paste("Created", date()))
    ncdf4::ncatt_put(ncout,0,"source",paste0("FACTS: emulandice.",facts_ssp,".emu",i_s,".emulandice.",reg))
    ncdf4::ncatt_put(ncout,0,"baseyear", baseyear)
    ncdf4::ncatt_put(ncout,0,"scenario",facts_ssp)
    ncdf4::ncatt_put(ncout,0,"region",paste(i_s, reg, sep = "."))

    # Close
    ncdf4::nc_close(ncout)

  }


  cat("\n_____________________________________\n",file = logfile_results, append = TRUE)

}
