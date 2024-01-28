#' plot_timeseries: timeseries plots.
#'
#' @description
#' Timeseries figures of simulations and/or projections.
#'
#' @param data_type Data type: "sims" for simulations; "prior" or "posterior" for
#' emulator projections before or after calibration.
#' @param plot_level Plot level: 0 for none, 1 for main, 2 for exhaustive
#'
#' @export

# XXX Tidy prior quantiles: don't plot zoom for plot_level >= 2
# XXX Increase y-axis for mean and final plots

plot_timeseries <- function(data_type, plot_level = 0) {

  # Previous plots were 2x1
  par(mfrow = c(1,1))

  # Reset graphical parameters
  par(xpd = FALSE, mar = c(5, 4, 4, 2) + 0.1)

  # Sims (and emulation) --------------------------------------------------------------------

  #______________________________
  if (plot_level >= 2) {

    # Loops over all scenarios for simulations when called from emulator_build.R
    # or one scenario with emulator projections when called from main.R
    for (scen in scenario_list) {

      # When plotting emulator projections, do both mean and final
      for (proj_type in c("mean", "final")) {

        # Skip final if just plotting sims
        if (data_type == "sims" && proj_type == "final") next

        for (plot_scale in (c("full", "zoom"))) {

          if (data_type %in% c( "sims", "prior")) {

            xstart <- first_year
            xend <- ifelse(plot_scale == "zoom", 2050, final_year)

            # Range for yaxis
            ymin <- sle_lim[[ as.character(xend) ]][1]
            ymax <- sle_lim[[ as.character(xend) ]][2]

            # For legend lines
            if (plot_scale == "full") {
              line_len <- 10
              leg_off <- 5
            }
            if (plot_scale == "zoom") {
              line_len <- 4
              leg_off <- 1
            }

            # Change title if just simulations
            if (data_type == "sims") {
              plot_title <- paste0( ice_name, ": ", scen_name[[scen]], " sims")
            } else plot_title <- paste0( ice_name, ": ", scen_name[[scen]], " ", proj_type)

            # PLOT: individual mean projections
            plot( 1:3, 1:3, type = "n", xlim = c(xstart, xend), ylim = c(ymin, ymax),
                  xaxs = "i", yaxs = "i",
                  main = plot_title, # dataset
                  xlab = "Year", ylab = paste("Sea level contribution relative to",cal_start,"(cm SLE)") )
            abline( h = 0 , lwd = 0.5)

            # Baseline and calibration period
            abline( v = cal_start, lwd = 0.5, lty = 2 )
            if (data_type != "sims") {
              abline( v = cal_end, lwd = 0.5, lty = 2 )
            }

            # Plot emulator projections (timeslices every nyrs)
            if (data_type == "prior") {

              # Note add zero value at cal_start xxx -> emulator_predict
              if (proj_type == "mean") apply( myem[[scen]]$mean, 1, function(x) lines( c(cal_start, years_em), c(0,x),
                                                                                       col = grey(0.8, alpha = 0.4 )) )
              if (proj_type == "final") apply( projections[[scen]], 1, function(x) lines( c(cal_start, years_em), c(0,x),
                                                                                          col = grey(0.8, alpha = 0.4 )) )

              # Legend
              yleg <- 0.80*ymax
              lines( leg_off + (xstart + 1):(xstart + line_len), rep(yleg, line_len), lwd = 1.5, col = grey(0.8, alpha = 0.6) )
              text(x = leg_off + (xstart + line_len), y = yleg, pos = 4, "Emulator projections")

            }

            # Plot original simulations
            if (data_type %in% c( "sims", "prior")) {

              # commented out 19/12/23 because no emulator
             # if (design_name %in% c("AR6_2LM", "unif_temps")) {

                scen_list <- scen # Was NA, but this should catch other SSPs
                if (scen == "SSP126") scen_list <- c("RCP26", "SSP126")
                if (scen == "SSP245") scen_list <- c("RCP45", "SSP245")
                if (scen == "SSP585") scen_list <- c("RCP85", "SSP585")

                # If find simulations for scenario(s):
                if (length(ice_data[ ice_data$scenario %in% scen_list, ]) > 0) {
                  apply( ice_data[ ice_data$scenario %in% scen_list, paste0("y", years_sim) ],
                         1, function(x) lines( years_sim, x, col =  AR6_rgb[[scen]], lwd = 0.3) )

                  # Legend
                  yleg <- 0.90*ymax
                  lines( leg_off + (xstart + 1):(xstart + line_len), rep(yleg, line_len), lwd = 1.5, col = AR6_rgb[[scen]] )
                  text(x = leg_off + (xstart + line_len), y = yleg, pos = 4, "Simulations")
                }

              #}

            }

            #if (i_s %in% c( "GIS", "AIS")) { # "GrIS",

            # Observations last to be clearer:
            # Offset so mean in cal_start is zero

            # Mean
            lines( obs_data[,"Year"], obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                   col = "black", lwd = 1.5) # grey(0.2, 0.4)
            # Mean +- 3 x obs_error
            if (plot_scale == "zoom") {
              polygon( c(obs_data[,"Year"], rev(obs_data[,"Year"])),
                       c( obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[,"SLE_sd"],
                          rev(obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[,"SLE_sd"])),
                       col = grey(0.2,0.05), border = "black", lwd = 0.5, lty = 2)
            }


              # mean +- 3 x total_error
              polygon( c(obs_data[,"Year"], rev(obs_data[,"Year"])),
                       c( obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err,
                          rev(obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err)),
                       col = grey(0.2,0.05), border = "black", lwd = 0.5, lty = 1)

            #}

          } # sims, prior

        } # scale zoom or full
      } # proj type


    } # scenario loop

  } # plot_level >= 2


  # Noise --------------------------------------------------------------------

  if (data_type == "prior") {

    if (plot_level >= 2) {

      # Currently switched off
      if (FALSE) {

        for (scen in scenario_list) {

          # EXAMPLE OF ADDING NOISE PROJECTIONS
          # Show example for first GSAT sample (n=1)
          plot(c(cal_start, years_em), c(0,myem[[scen]]$mean[1, ]), type = "l",
               col = AR6_rgb[[scen]],
               main = paste("Emulator uncertainty example:", scen_name[[scen]]),
               ylim = ylim[[as.character(final_year)]],
               xlab = "Year", ylab = "Sea level contribution (cm SLE)")
          polygon( c(cal_start, years_em, rev(years_em), cal_start),
                   c(0, myem[[scen]]$mean[1, ] + 2 * myem[[scen]]$sd[1, ],
                     rev(myem[[scen]]$mean[1, ] - 2 * myem[[scen]]$sd[1, ] ), 0),
                   border = AR6_rgb[[scen]])

          for (nn in 1:100) {
            noise <- rt(1, df = N_sims-N_ts)
            ts_sample <- myem[[scen]]$mean[1, ] + noise * myem[[scen]]$sd[1, ]
            lines( c(cal_start, years_em), c(0, ts_sample), lwd = 0.5, col = grey(0.3, alpha = 0.4) )
          }
        }

      }
    } # all plots

  } # prior



}
