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

# XXX Tidy sims SSP time series plot: don't plot for final, change title
# XXX Tidy prior quantiles: don't plot zoom for plot_level >= 2
# XXX Increase y-axis for mean and final plots

plot_timeseries <- function(data_type, plot_level = 0) {

#  print(paste("Timeseries plots:", data_type))
  par(mfrow = c(1,1))

  #___________________________________________________________________________
  # QUANTILE PLOT

  # pdf( file = paste0( output_dir, "/MAIN_timeseries_IPCC.pdf" ),
  # width = 14, height = 6.5)

  for (plot_scale in (c("full","zoom"))) {
  # for (plot_scale in yy_plot) { # do this

    if ( data_type == "prior" || (data_type == "sims" && plot_level >= 2) ) {

      # bottom, left, top, right
      rr = length(scenario_list) + 2.5
      par(xpd = NA, mar = c(5, 5, 1, rr))

      if (plot_scale == "full") {

#        xstart <- ifelse(dataset == "IPCC_AR6", 1990, 1950)
        xstart <- 1950
        xend <- 2300

        # Use default ranges for 2300 full plot
        ymin = ylim_max[1]
        ymax = ylim_max[2]

        bpx = xend + 10
        bpinc <- 3

        # If only projecting to 2100ish
        if ( final_year <= 2150) { # (i_s == "GIS" && ensemble_subset == "all") || (i_s == "GLA" && ensemble_subset == "forcing")) {
          xend <- final_year
          ymin <- ylim[1]
          ymax <- ylim[2]
          bpx = xend + 3
          bpinc <- 1

        }

      }

      # Zoom scales: hard-code these
      if (plot_scale == "zoom") {

        xstart <- years_sim[1]
        xend <- 2030
        if (i_s == "AIS") { #xstart <- 1950;
        ymin <- -10; ymax <- 15 }
        if (i_s == "GIS") { #xstart <- 1995;
          ymin <- -1; ymax <- 5 }
        if (i_s == "GLA") { #xstart <- 1980;
          ymin <- -0.5; ymax <- 2 }

        bpx = xend + 2 # was 5
        bpinc <- 0.7

      }

      tsize <- 0.9

      # PLOT QUANTILES
      plot( 1:3, 1:3, type = "n", xlim = c(xstart, xend), ylim = c(ymin, ymax),
            xaxs = "i", yaxs = "i", main = NULL,
            xlab = "Year", ylab = paste("Sea level contribution relative to",cal_start,"(cm SLE)"),
            cex = tsize, cex.axis = tsize, cex.lab = tsize, cex.main = tsize)
      lines( c(xstart, xend), c(0,0), lwd = 0.5)
      lines( rep(cal_start, 2), c(ymin, ymax), lwd = 0.5, lty = 5)
      lines( rep(cal_end, 2), c(ymin, ymax), lwd = 0.5, lty = 5)

      # Max glacier line
      if (i_s == "GLA") lines( c(xstart, xend), rep(max_glaciers[[reg]], 2), lwd = 0.5, lty = 5)

      bpy = ymax*0.8 #- 5

      # xxx add glacier obs!
      # if (i_s %in% c( "GrIS", "GIS", "AIS")) {

        # Plot IMBIE, offset so mean in cal_start is zero
        # mean
        lines( obs_data[,"Year"], obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
               col = grey(0.2, 0.4), lwd = 1.5)

        # mean +- 3 x obs_error (if zoom)
        if (plot_scale == "zoom") {
          polygon( c(obs_data[,"Year"], rev(obs_data[,"Year"])),
                   c( obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
                      + 3 * obs_data[,"SLE_sd"],
                      rev(obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
                          - 3 * obs_data[,"SLE_sd"])),
                   col = grey(0.2,0.05), border = "black", lwd = 0.5, lty = 2)
        }

        # mean +- 3 x total_error
        polygon( c(obs_data[,"Year"], rev(obs_data[,"Year"])),
                 c( obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
                    + 3 * total_err,
                    rev(obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"]
                        - 3 * total_err)),
                 col = grey(0.2,0.05), border = grey(0.2, 0.4), lwd = 1, lty = 1) # 3 if zoom

        # legend
        #yleg <- 0.7*ylim[2]
        text(x = xstart + 2, y = bpy, pos = 4, "Observations",
             col = grey(0.2, 0.4), cex = tsize*0.9)
        #lines( 2016 + (1:5), rep(bpy, 5), col = grey(0.2, 0.4), lwd = 1.5)
        #rect( 2017, bpy - 0.2*ylim[2], 2021, bpy + 0.2*ylim[2], # 0.02 if zoom
        #      col = grey(0.2,0.05), border = NA)
     # }

      # Only add emulator projections if they exist
      if (data_type == "prior") {

        # Add each quantile polygon
        for (scen in scenario_list) {

          # 5-95% range
          polygon( x = c(cal_start, years_em[years_em <= xend], rev( years_em[years_em <= xend] ), cal_start),
                   y = c(0, projections_quant[[scen]][ q_list == 0.95, paste0("y", years_em[years_em <= xend] )],
                         rev(projections_quant[[scen]][ q_list == 0.05, paste0("y", years_em[years_em <= xend] )]), 0),
                   col = AR6_rgb_light[[scen]], border = NA )

          # Median: leave out for now for clarity
          #        lines( c(cal_start, years_em), c(0, projections_quant[[scen]][ q_list == 0.5, ]),
          #               col = AR6_rgb[[scen]], lwd = 2)

          # SSP names in legend
          bpy = bpy*0.9 #- 2 # was -4 for land ice
          text(xstart + 2, bpy, scen_name[[scen]], pos = 4, col = AR6_rgb[[scen]], cex = tsize*0.9)

          # Box plots at righthand side for final year
          arrows( bpx, projections_quant[[scen]][ q_list == 0.05, paste0("y", xend) ],
                  bpx, projections_quant[[scen]][ q_list == 0.95, paste0("y", xend) ],
                  code = 3, length = 0.08, angle = 90,
                  col = AR6_rgb[[scen]])
          rect(bpx - bpinc, projections_quant[[scen]][ q_list == 0.25, paste0("y", xend) ],
               bpx + bpinc, projections_quant[[scen]][ q_list == 0.75, paste0("y", xend) ],
               col = "white", border = AR6_rgb[[scen]])
          lines( bpx + c(-bpinc, bpinc),
                 rep(projections_quant[[scen]][ q_list == 0.5, paste0("y", xend) ], 2 ), col = AR6_rgb[[scen]])

          if (plot_scale == "full") {
            if (xend == 2100) {
              bpx <- bpx + 4
            } else bpx = bpx + 10
          }
          if (plot_scale == "zoom") bpx <- bpx + 2 # 3

        } # scenarios

      } # prior

      # Plot simulations faintly over the top
      if (design_name %in% c("AR6_2LM", "unif_temps")) {
        # xxx tidy: years_to_plot <- years_sim[years_sim <= xend])
        if ("SSP126" %in% scenario_list) apply( ice_data[ ice_data$scenario %in% c("RCP26", "SSP126"),
                                                          paste0("y", years_sim[years_sim <= xend]) ], 1,
                                                function(x) lines( years_sim[years_sim <= xend] , x, col = AR6_rgb_light[["SSP126"]], lwd = 0.2 ))
        if ("SSP245" %in% scenario_list) apply( ice_data[ ice_data$scenario %in% c("RCP45", "SSP245"),
                                                          paste0("y", years_sim[years_sim <= xend] ) ], 1,
                                                function(x) lines( years_sim[years_sim <= xend], x, col = AR6_rgb_light[["SSP245"]], lwd = 0.2 ))
        if ("SSP585" %in% scenario_list) apply( ice_data[ ice_data$scenario %in% c("RCP85", "SSP585"),
                                                          paste0("y", years_sim[years_sim <= xend]) ], 1,
                                                function(x) lines( years_sim[years_sim <= xend], x, col = AR6_rgb_light[["SSP585"]], lwd = 0.2 ))
      } # add simulations

      # LAND ICE: REGION graph title
      # ymax - 5 for land ice
      text( xstart + 2, ymax *0.9, pos = 4, paste0(ice_name), cex = 1.6 ) # ymax - 2

    } # data_type

  } # plot_scale

  # Reset graphical parameters
  par(xpd = FALSE, mar = c(5, 4, 4, 2) + 0.1)


  #______________________________
  if (plot_level >= 2) {

    for (scen in scenario_list) {

      # xxx add this structure to other plots?
      for (proj_type in c("mean", "final")) { # xxx skip final and change mean title if sims

        for (plot_scale in (c("full", "zoom"))) {

          if (data_type %in% c( "sims", "prior")) {

            if (plot_scale == "full") {

             # xstart <- ifelse(dataset == "IPCC_AR6", 1990, 1950)
              xstart <- 1950
              xend <- 2300

              # Use default ranges for 2300 full plot
              ymin = ylim_max[1]
              ymax = ylim_max[2]

              # 2100ish only
              if ( final_year <= 2150) { # if ( (i_s == "GIS" && ensemble_subset == "all") || (i_s == "GLA" && ensemble_subset == "forcing")) {
                xend <- final_year
                ymin <- ylim[1]
                ymax <- ylim[2]
              }
            }

            # Zoom scales: hard-code these
            if (plot_scale == "zoom") {

              xend <- 2030
              ymin <- ylim_max[1] * 0.7
              ymax <- ylim_max[2] * 0.8
              if (i_s == "AIS") { xstart <- 1950 } # ymin <- -10} # ymax <- 15 }
              if (i_s == "GIS") { xstart <- 1995 } #ymin <- -1} # ymax <- 5 }
              if (i_s == "GLA") { xstart <- 1980 } # ymin <- -0.5} # ymax <- 2 }
            }

            #if (plot_scale == "full") {
            #  ylim_2 <- ylim # aarghh
            #  xlim_2 <- c(1960, 2300)
            #  if (dataset == "IPCC_AR6") xlim_2[1] <- 1990
            #  if (i_s == "GLA") xlim_2 <- c(1980, 2300) # xxx aaargh
            #}

            # if (plot_scale == "zoom") {
            #ylim_2 <- c(-2, 3)
            #xlim_2 <- c(1960, 2030)
            #if (dataset == "IPCC_AR6") xlim_2[1] <- 1990
            #if (i_s == "GLA") {
            #  xlim_2[1] <- 1980 # xxx aaargh
            #  ylim_2 <- ylim # xxx aaaaaarhj
            #}
            #}

            # PLOT: individual mean projections
            plot( 1:3, 1:3, type = "n", xlim = c(xstart, xend), ylim = c(ymin, ymax),
                  xaxs = "i", yaxs = "i",
                  main = paste( ice_name, ":", proj_type, scen_name[[scen]]), # dataset
                  xlab = "Year", ylab = paste("Sea level contribution relative to",cal_start,"(cm SLE)") )
            abline( h = 0 , lwd = 0.5)
            abline( v = cal_start, lwd = 0.5 ) # zero
            abline( v = cal_end, lwd = 0.5 ) # zero


            # XXX FIX YLEG AND ADD BACK; no _2
            # legend
            yleg <- 0.7*ymax
            # xxx cal_start - 15:20 ish
            #        lines( xlim_2[1] + 1:5, rep(yleg, 5), col = grey(0.2, 0.4), lwd = 1.5)
            #       rect( xlim_2[1] + 1, yleg - 0.02*ylim[2], xlim_2[1] + 5, yleg + 0.02*ylim_2[2],
            #            col = grey(0.2,0.05), border = NA)
            #      text(x = xlim_2[1], y = yleg, pos = 4, "IMBIE observations")


            if (data_type == "prior") {

              # Add emulator projections (timeslices every nyrs)
              # Add zero value at cal_start xxx Future: do this in emulator_predict
              if (proj_type == "mean") apply( myem[[scen]]$mean, 1, function(x) lines( c(cal_start, years_em), c(0,x),
                                                                                       col = grey(0.8, alpha = 0.4 )) )
              if (proj_type == "final") apply( projections[[scen]], 1, function(x) lines( c(cal_start, years_em), c(0,x),
                                                                                          col = grey(0.8, alpha = 0.4 )) )

              # legend
              yleg <- 0.80*ymax
              lines( 1990:1999, rep(yleg, 10), lwd = 1.5, col = grey(0.8, alpha = 0.6) )
              text(x = 1999, y = yleg, pos = 4, "Emulator projections")

            }

            if (data_type %in% c( "sims", "prior")) {

              # Plot original simulations
              if (design_name %in% c("AR6_2LM", "unif_temps")) {
                yleg <- 0.90*ymax

                scen_list <- NA
                if (scen == "SSP126") scen_list <- c("RCP26", "SSP126")
                if (scen == "SSP245") scen_list <- c("RCP45", "SSP245")
                if (scen == "SSP585") scen_list <- c("RCP85", "SSP585")

                apply( ice_data[ ice_data$scenario %in% scen_list, paste0("y", years_sim) ],
                       1, function(x) lines( years_sim, x, col =  AR6_rgb[[scen]], lwd = 0.3) )
                lines( 1990:1999, rep(yleg, 10), lwd = 1.5, col = AR6_rgb[[scen]] )
              }
              #              if (scen == "SSP585") {
              #                apply( ice_data[ ice_data$scenario %in% scen_list, paste0("y", years_sim) ], 1, function(x) lines( years_sim, x, col = rgb(1,0,0, alpha = 0.4 )) )
              #                lines( 1991:1999, rep(yleg, 5), lwd = 1.5, col = rgb(1,0,0, alpha = 0.4 ) )
              #              }
              # legend
              text(x = 1999, y = yleg, pos = 4, "Simulations")

            }

            if (i_s %in% c( "GIS", "AIS")) { # "GrIS",

              # IMBIE last to be clearer:

              # Add IMBIE, offset so mean in cal_start is zero
              # mean
              lines( obs_data[,"Year"], obs_data[,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                     col = "black", lwd = 1.5) # grey(0.2, 0.4)
              # mean +- 3 x model_error
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

            }

          }

        } # scale zoom or full
      } # proj type


    } # scenario loop

  } # plot_level >= 2


  # Noise --------------------------------------------------------------------

  if (data_type == "prior") {

    if (plot_level >= 2) {

      if (FALSE) {

        #print("Noise plots")

        for (scen in scenario_list) {

          # EXAMPLE OF ADDING NOISE PROJECTIONS
          # Show example for first GSAT sample (n=1)
          plot(c(cal_start, years_em), c(0,myem[[scen]]$mean[1, ]), type = "l",
               col = AR6_rgb[[scen]],
               main = paste("Emulator uncertainty example:", scen_name[[scen]]),
               ylim = ylim,
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
