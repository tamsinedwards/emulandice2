#' plot_scatter: plot scatter (X-Y) figures.
#'
#' @description Plot scatter and X-Y plots, including main effects.
#'
#' @param data_type Data type: "sims" for simulations; "prior" or "posterior" for
#' emulator projections before or after calibration.
#' @param design_name Some plots are for main effects or projections only; some only for sims
#' Takes values none (sims); main_effects; unif_temps or AR6_2LM (projections)
#' @param plot_level Plot level: 0 for none, 1 for main, 2 for exhaustive

#' @export

# Not writing to logfile because called from both build and main
plot_scatter <- function(data_type, design_name, plot_level = 0) {

  # Prediction designs
  # Main_effects and unif_temps are used by emulator_build.R for emulator validation,
  # while AR6_2LM is used by main.R to predict
  stopifnot(design_name %in% c("none","main_effects", "unif_temps", "AR6_2LM"))

  par(mfrow = c(1,2), pin = c(2.7,2.7), cex.main = 0.6, cex.axis = 0.7, cex.lab = 0.7)

  for (scen in scenario_list) {

    for (yy in yy_plot ) {

      if (yy == cal_end) next # because plotting vs cal_end


      # * Future vs past: mean ------------------------------------------------------------

      if (data_type == "prior") {

        # PLOT CALIBRATION SCATTER: FUTURE VS PAST - mean [ option: +/- 3 s.d. error bars ]
        plot(1:3, 1:3, type = "n",
             main = paste0( ice_name, " ", yy, " vs ", cal_end,": ", scen_name[[scen]],
                            " mean"), # xxx fix name when sims only; ditch 5-95% intervals
             xlim = ylim_obs, ylim = sle_lim[[yy]], xaxs = "i", yaxs = "i",
             cex.main = 0.7,
             xlab = paste("Sea level contribution from",cal_start,"-",cal_end,"(cm)"),
             ylab = paste("Sea level contribution from",cal_start,"-",yy,"(cm)"))
        abline( h = 0 )
        if (i_s == "GLA") {
          abline( h = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
        }

        # Plot observations
        abline( v = obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                col = grey(0.2, 0.4), lwd = 1.6)
        rect( obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
              sle_lim[[yy]][1],
              obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
              sle_lim[[yy]][2],
              col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
        if (plot_level > 2) {
          rect( obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err[obs_data$Year == cal_end],
                sle_lim[[yy]][1],
                obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err[obs_data$Year == cal_end],
                sle_lim[[yy]][2],
                col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
        }

        if (data_type == "prior") {

          # Emulated
          points(myem[[scen]]$mean[ , paste0("y",cal_end) ],
                 myem[[scen]]$mean[ , paste0("y", yy) ], cex = 0.5,
                 pch = 16, col = AR6_rgb_light[[scen]])

          # Add error bars: +/- 2 s.d.? NO BECAUSE SWITCHED TO FULL COVAR? XXX CHECK

          # Horizontal
          arrows( myem[[scen]]$mean[ , paste0("y",cal_end) ] - 2 * myem[[scen]]$sd[ , paste0("y",cal_end) ],
                  myem[[scen]]$mean[ , paste0("y", yy) ],
                  myem[[scen]]$mean[ , paste0("y",cal_end) ] + 2* myem[[scen]]$sd[ , paste0("y",cal_end) ],
                  myem[[scen]]$mean[ , paste0("y", yy) ],
                  code = 3, length = 0.08, angle = 90, lwd = 0.1,
                  col = AR6_rgb_light[[scen]])

          # Vertical
          arrows( myem[[scen]]$mean[ , paste0("y",cal_end) ],
                  myem[[scen]]$mean[ , paste0("y", yy) ] - 2 * myem[[scen]]$sd[ , paste0("y", yy) ],
                  myem[[scen]]$mean[ , paste0("y",cal_end) ],
                  myem[[scen]]$mean[ , paste0("y", yy) ] + 2 * myem[[scen]]$sd[ , paste0("y", yy) ],
                  code = 3, length = 0.08, angle = 90, lwd = 0.1,
                  col = AR6_rgb_light[[scen]])

          yleg <- 0.90*sle_lim[[yy]][2]
          points( ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), yleg, pch = 16, col = AR6_rgb_light[[scen]], cex = 0.7)
          text(x = ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), y = yleg, pos = 4, "Emulated mean +/- 2 s.d.", cex = 0.7)

        } # prior only

        # ADD SIMULATED
        yleg <- 0.82*sle_lim[[yy]][2]

        # Note AR6_2LM should not necessarily overlap simulations due to sampling of GSAT
        if (design_name %in% c("AR6_2LM","unif_temps")) {

          plot_data <- NA
          if (scen == "SSP126") plot_data <- ice_data[ ice_data$scenario %in% c("RCP26", "SSP126"), ]
          if (scen == "SSP245") plot_data <- ice_data[ ice_data$scenario %in% c("RCP45", "SSP245"), ]
          if (scen == "SSP585") plot_data <- ice_data[ ice_data$scenario %in% c("RCP85", "SSP585"), ]

          # Assume more than one simulation...
          if ( length(plot_data) > 1 ) {
            apply( plot_data, 1,
                   function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y",yy) ],
                                       pch = 16, cex = 0.5, col = "black" ) )
            points( ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), yleg,
                    pch = 16, cex = 0.7, col = "black" )
            text(x = ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), y = yleg, pos = 4, "Simulated", cex = 0.7)

          }

        } # design name

        # xxx Future: code scenario and colour instead of duplicating code? and above too
        #if (design_name %in% c("AR6_2LM","unif_temps")) {
        #  if (scen == "SSP126") {
        #    apply( ice_data[ ice_data$scenario %in% c("RCP26", "SSP126"), ], 1,
        #           function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y", yy) ], pch = 16,
        #                               cex = 0.8, col = "black" ) ) #AR6_rgb[[scen]] ) )
        #  }
        #  if (scen == "SSP245") {
        #    apply( ice_data[ ice_data$scenario %in% c("RCP45", "SSP245"), ], 1,
        #           function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y", yy) ], pch = 16,
        #                               cex = 0.8, col = "black" ) ) #AR6_rgb[[scen]] ) )
        #  }
        #  if (scen == "SSP585") {
        #    apply( ice_data[ ice_data$scenario %in% c("RCP85", "SSP585"), ], 1,
        #           function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y", yy) ], pch = 16,
        #                               cex = 0.8, col = "black" ) ) # AR6_rgb[[scen]] ) )
        #  }

        # Legend dot
        # xxx use ylim_obs[1]
        #  points( -2, yleg, pch = 16, cex = 0.8, col = "black" ) # AR6_rgb[[scen]] )
        #  text(x = -2, y = yleg, pos = 4, "Simulator")
        #}

      } # prior

      # * Future vs past: final ------------------------------------------------------------

      #___________________________________________________________________________
      # PLOT CALIBRATION SCATTER: FUTURE VS PAST - final

      # Posterior because then shows NROY as darker
      if (data_type == "posterior") {

        plot(1:3, 1:3, type = "n",
             main = paste0( ice_name, " ", yy, " vs ", cal_end,": ", scen_name[[scen]], " final"),
             xlim = ylim_obs, ylim = sle_lim[[yy]], xaxs = "i", yaxs = "i",
             xlab = paste("Sea level contribution from",cal_start,"-",cal_end,"(cm)"),
             ylab = paste("Sea level contribution from",cal_start,"-",yy,"(cm)"))
        abline( h = 0, lwd = 0.2, col = "darkgrey" )
        if (i_s == "GLA") {
          abline( h = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
        }

        # Observations
        abline( v = obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                col = grey(0.2, 0.4), lwd = 1.6)
        rect( obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
              sle_lim[[yy]][1],
              obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
              sle_lim[[yy]][2],
              col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
        if (plot_level > 2) {
          rect( obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err[obs_data$Year == cal_end],
                sle_lim[[yy]][1],
                obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err[obs_data$Year == cal_end],
                sle_lim[[yy]][2],
                col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
        }
        # EMULATED
        points(projections[[scen]][ , paste0("y",cal_end) ],
               projections[[scen]][ , paste0("y",yy) ], pch = 16, cex = 0.5,
               col = AR6_rgb_light[[scen]], bg = AR6_rgb_light[[scen]])

        yleg <- sle_lim[[yy]][1] + 0.87*(sle_lim[[yy]][2] - sle_lim[[yy]][1])
        points( ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), yleg, pch = 16, cex = 0.7,
                col = AR6_rgb_light[[scen]], bg = AR6_rgb_light[[scen]] )
        text(x = ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), y = yleg, pos = 4, "Emulated", cex = 0.7)

        # Plot calibrated (NROY) again but darker
        # Only do this for real projections, to avoid calibrating unif_temps for visualisation only
        if (design_name == "AR6_2LM") {
          points(projections[[scen]][ proj_nroy[[scen]], paste0("y",cal_end) ],
                 projections[[scen]][ proj_nroy[[scen]], paste0("y",yy) ],
                 pch = 16, cex = 0.5, bg = AR6_rgb_light[[scen]],
                 col = AR6_rgb_med[[scen]])
          yleg <- sle_lim[[yy]][1] + 0.82*(sle_lim[[yy]][2] - sle_lim[[yy]][1])
          points( ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), yleg, pch = 16, cex = 0.7,
                  col = AR6_rgb_med[[scen]], bg = AR6_rgb_med[[scen]] )
          text(x = ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), y = yleg, pos = 4, "Emulated: NROY", cex = 0.7)
        }

        # SIMULATIONS
        yleg <- sle_lim[[yy]][1] + 0.92*(sle_lim[[yy]][2] - sle_lim[[yy]][1])

        # Note AR6_2LM should not necessarily overlap simulations due to sampling of GSAT
        if (design_name %in% c("AR6_2LM","unif_temps")) {

          plot_data <- NA
          if (scen == "SSP126") plot_data <- ice_data[ ice_data$scenario %in% c("RCP26", "SSP126"), ]
          if (scen == "SSP245") plot_data <- ice_data[ ice_data$scenario %in% c("RCP45", "SSP245"), ]
          if (scen == "SSP585") plot_data <- ice_data[ ice_data$scenario %in% c("RCP85", "SSP585"), ]

          # Assume more than one simulation...
          if ( length(plot_data) > 1 ) {
            apply( plot_data, 1,
                   function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y",yy) ],
                                       pch = 16, cex = 0.5, col = "black" ) )
            points( ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), yleg,
                    pch = 16, cex = 0.7, col = "black" )
            text(x = ylim_obs[1] + 0.05*(ylim_obs[2] - ylim_obs[1]), y = yleg, pos = 4, "Simulated", cex = 0.7)

          }

        } # design name
      } # prior/posterior xxx just posterior now?
    }  # year list yy_plot

  } # scenario_list

  if (plot_level >= 2) {

    # EMULATOR
    if (data_type == "prior") { #%in% c("prior","posterior")) {

      # NOTE THIS SHOULD REALLY BE CAL_END - CAL_START
      # BUT WORKS BECAUSE CAL_START IS ALWAYS ZERO FOR NOW
      # [does this refer to yy or plot?]
      for (yy in yy_plot) {

        for (scen in scenario_list) {

          # SEA LEVEL VS TEMP TIMESLICES
          # Mean projections
          for (gg in temps_list_names) {

            # SLE vs GSAT: mean ------------------------------------------------------------

            plot( design_pred[[scen]][,gg], myem[[scen]]$mean[,paste0("y",yy)],
                  pch = 16, col = AR6_rgb_light[[scen]], # grey(0.8, alpha = 0.4 ),
                  main = paste( "Mean projections at",yy,"for", scen_name[[scen]] ),
                  xlab = GSAT_lab[[gg]],
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  ylim = sle_lim[[yy]])
            abline( h = 0 )

            if (yy == cal_end) {
              abline( h = obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                      col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(design_pred[[scen]][,gg]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    max(design_pred[[scen]][,gg]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(design_pred[[scen]][,gg]),
                      obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err[obs_data$Year == cal_end],
                      max(design_pred[[scen]][,gg]),
                      obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err[obs_data$Year == cal_end],
                      col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }

            # Error bars
            arrows( design_pred[[scen]][,gg],
                    myem[[scen]]$mean[ , paste0("y", yy) ] - 2 * myem[[scen]]$sd[ , paste0("y", yy) ],
                    design_pred[[scen]][,gg],
                    myem[[scen]]$mean[ , paste0("y", yy) ] + 2 * myem[[scen]]$sd[ , paste0("y", yy) ],
                    code = 3, length = 0.08, angle = 90,
                    col = AR6_rgb_light[[scen]])

            # xxx Note this excludes any RCPs!!
            if (length(temps_list) == 1) {
              points( temps[ice_data$scenario == scen], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                      pch = 16, col = AR6_rgb[[scen]], cex = 0.8)
            } else points( temps[ice_data$scenario == scen, gg], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                           pch = 16, col = AR6_rgb[[scen]], cex = 0.8)

            # SLE vs GSAT: full ------------------------------------------------------------
            # Full projections

            plot( design_pred[[scen]][,gg], projections[[scen]][,paste0("y",yy)],
                  pch = 16, col = AR6_rgb_light[[scen]], # grey(0.8, alpha = 0.4),
                  main = paste( "Final projections at",yy,"for", scen_name[[scen]] ),
                  xlab = GSAT_lab[[gg]],
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  ylim = sle_lim[[yy]])
            abline( h = 0 )

            if (yy == cal_end) {
              abline( h = obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                      col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(design_pred[[scen]][,gg]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    max(design_pred[[scen]][,gg]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(design_pred[[scen]][,gg]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err[obs_data$Year == cal_end],
                    max(design_pred[[scen]][,gg]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err[obs_data$Year == cal_end],
                    col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }

            # xxx Note this excludes any RCPs!!
            if (length(temps_list) == 1) {
              points( temps[ice_data$scenario == scen], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                      pch = 16, col = AR6_rgb[[scen]], cex = 0.7)
            } else points( temps[ice_data$scenario == scen, gg], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                           pch = 16, col = AR6_rgb[[scen]], cex = 0.7)

          } # GSAT loop

          # SLE vs ice inputs: mean ------------------------------------------------------------

          # SEA LEVEL VS ICE MODEL PARAMETER
          # Plot mean and full projections vs each parameter in turn
          for (pp in ice_all_list) {

            plot( design_pred[[scen]][,pp], myem[[scen]]$mean[,paste0("y",yy)],
                  pch = 16, col = AR6_rgb_light[[scen]],
                  main = paste("Mean projections at",yy,"for", scen_name[[scen]]),
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  xlab = pp, ylim = sle_lim[[yy]])
            abline( h = 0 )

            if (yy == cal_end) {
              abline( h = obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                      col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    max(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err[obs_data$Year == cal_end],
                    max(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err[obs_data$Year == cal_end],
                    col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }

            arrows( design_pred[[scen]][,pp],
                    myem[[scen]]$mean[ , paste0("y", yy) ] - 2 * myem[[scen]]$sd[ , paste0("y", yy) ],
                    design_pred[[scen]][,pp],
                    myem[[scen]]$mean[ , paste0("y", yy) ] + 2 * myem[[scen]]$sd[ , paste0("y", yy) ],
                    code = 3, length = 0.08, angle = 90,
                    col = AR6_rgb_light[[scen]])

            # xxx Note this excludes RCPs!
            # xxx was ice_param
            points( unlist(ice_design[,pp])[ice_data$scenario == scen], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                    pch = 16, col = AR6_rgb[[scen]], cex = 0.7)

          } # param list

          # SLE vs ice inputs: final ------------------------------------------------------------

          for (pp in ice_all_list) {

            # SAME AGAIN BUT FINAL PROJECTIONS

            plot( design_pred[[scen]][,pp], projections[[scen]][,paste0("y",yy)],
                  pch = 16, cex = 0.8, col = AR6_rgb_light[[scen]], # grey(0.8, alpha = 0.4),
                  main = paste("Final projections at",yy,"for", scen_name[[scen]]),
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  xlab = pp, ylim = sle_lim[[yy]] )
            abline( h = 0 )

            # xxx Note this excludes any RCPs!!
            # was ice_param
            points( unlist(ice_design[,pp])[ice_data$scenario == scen], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                    pch = 16, col = AR6_rgb[[scen]], cex = 0.8 )

            # ADD OBSERVATIONS
            if (yy == cal_end) {
              abline( h = obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"],
                      col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    max(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * obs_data[obs_data$Year == cal_end,"SLE_sd"],
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] - 3 * total_err[obs_data$Year == cal_end],
                    max(design_pred[[scen]][,pp]),
                    obs_data[obs_data$Year == cal_end,"SLE"] - obs_data[obs_data$Year == cal_start, "SLE"] + 3 * total_err[obs_data$Year == cal_end],
                    col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }
          } # param list

        } # SSP
      } # Year

    } # Prior only

  } # plot_level >= 2

  # * MAIN EFFECTS ------------------------------------------------------------

  # EMULATOR MAIN EFFECTS
  # Called from emulator_build.R
  if (design_name == "main_effects") {

    for (yy in yy_plot) {

      # SEA LEVEL VS TEMP
      # Mean projections

      for (gg in temps_list_names) {

        # gsat_year <- paste0("GSAT_", gg)

        col_darker <- rgb( 1, 0, 0, alpha = 0.4, maxColorValue = 1)
        col_paler <- rgb( 1, 0, 0, alpha = 0.2, maxColorValue = 1)

        # * Main effects: GSAT ------------------------------------------------------------
        plot( design_sa[[gg]][,gg], myem[[gg]]$mean[,paste0("y",yy)],
              type = "l", lwd = 1.2,
              main = paste( "Main effect: sea level at",yy,"vs", gg ),
              xlab = GSAT_lab[[gg]],
              ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
              ylim = sle_lim[[yy]] )
        abline( h = 0 )
        if (i_s == "GLA") {
          abline( h = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
        }

        polygon( c( design_sa[[gg]][,gg], rev( design_sa[[gg]][,gg] ) ),
                 c( myem[[gg]]$mean[ , paste0("y", yy) ] + myem[[gg]]$sd[ , paste0("y", yy) ],
                    rev( myem[[gg]]$mean[ , paste0("y", yy) ] - myem[[gg]]$sd[ , paste0("y", yy) ] ) ),
                 border = NA,
                 col = col_darker )
        polygon( c( design_sa[[gg]][,gg], rev( design_sa[[gg]][,gg] ) ),
                 c( myem[[gg]]$mean[ , paste0("y", yy) ] + 2 * myem[[gg]]$sd[ , paste0("y", yy) ],
                    rev( myem[[gg]]$mean[ , paste0("y", yy) ] - 2 * myem[[gg]]$sd[ , paste0("y", yy) ] ) ),
                 border = NA,
                 col = col_paler )

        leg_x1 <- min(design_sa[[gg]][,gg])
        leg_x2 <- max(design_sa[[gg]][,gg])
        rect( leg_x1, 0.94*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.9*sle_lim[[yy]][2], col = col_darker, border = NA)
        text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.92*sle_lim[[yy]][2], pos = 4, "Mean +/- 1 s.d.")
        rect( leg_x1, 0.8*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.75*sle_lim[[yy]][2], col = col_paler, border = NA)
        text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.78*sle_lim[[yy]][2], pos = 4, "Mean +/- 2 s.d.")

      } # GSAT loop

      # * Main effects: ice inputs (cont) ------------------------------------------------------------

      # SEA LEVEL VS ICE MODEL PARAMETERS

      col_list <- hcl.colors(length(ice_cont_list), palette = "Dark 3")

      for (pp in ice_cont_list) {

        col_rgb <- col2rgb( col_list[ which(ice_all_list == pp, arr.ind = TRUE)] )
        col_darker <- rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L], alpha = 0.4 * 255, maxColorValue = 255)
        col_paler <- rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L], alpha = 0.2 * 255, maxColorValue = 255)

        plot( design_sa[[pp]][,pp],
              myem[[pp]]$mean[,paste0("y",yy)],
              type = "l", lwd = 1.2,
              main = paste("Main effect: sea level at",yy,"vs", pp),
              ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
              xlab = pp, ylim = sle_lim[[yy]])
        abline( h = 0 )
        if (i_s == "GLA") {
          abline( h = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
        }

        polygon( c( design_sa[[pp]][,pp],
                    rev( design_sa[[pp]][,pp] ) ),
                 c( myem[[pp]]$mean[ , paste0("y", yy) ] + myem[[pp]]$sd[ , paste0("y", yy) ],
                    rev( myem[[pp]]$mean[ , paste0("y", yy) ] - myem[[pp]]$sd[ , paste0("y", yy) ] ) ),
                 border = NA,
                 col = col_darker )
        polygon( c( design_sa[[pp]][,pp],
                    rev( design_sa[[pp]][,pp]) ),
                 c( myem[[pp]]$mean[ , paste0("y", yy) ] + 2 * myem[[pp]]$sd[ , paste0("y", yy) ],
                    rev( myem[[pp]]$mean[ , paste0("y", yy) ] - 2 * myem[[pp]]$sd[ , paste0("y", yy) ] ) ),
                 border = NA,
                 col = col_paler )

        leg_x1 <- min(design_sa[[pp]][,pp])
        leg_x2 <- max(design_sa[[pp]][,pp])
        rect( leg_x1, 0.94*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.9*sle_lim[[yy]][2], col = col_darker, border = NA)
        text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.92*sle_lim[[yy]][2], pos = 4, "Mean +/- 1 s.d.")
        rect( leg_x1, 0.8*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.75*sle_lim[[yy]][2], col = col_paler, border = NA)
        text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.78*sle_lim[[yy]][2], pos = 4, "Mean +/- 2 s.d.")

      } # param list

      # Skip to next year plot if no factors
      if (length(ice_factor_list) == 1 && is.na(ice_factor_list)) next

      col_list <- hcl.colors(length(ice_factor_list), palette = "Dark 3")

      # * Main effects: ice inputs (fac) ------------------------------------------------------------

      for (pp in ice_factor_list) {

        plot( 1:3, 1:3, type = "n", xaxt = "n",
              main = paste("Main effect: sea level at",yy,"vs", pp),
              ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
              xlab = pp, xlim = c(0,length(ice_factor_values[[pp]])+1), ylim = sle_lim[[yy]], xaxs = "i")
        axis(side = 1, at = 1:length(ice_factor_values[[pp]]), labels = ice_factor_values[[pp]],
             cex.axis = 0.6, las = 3)
        abline( h = 0 )

        col_rgb <- col2rgb( col_list[ which(ice_factor_list == pp, arr.ind = TRUE)] )
        col_darker <- rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L], alpha = 0.4 * 255, maxColorValue = 255)

        # Levels for factor
        for (ll in 1:length(ice_factor_values[[pp]])) {

          # Box plot: whiskers
          lab <- paste0(pp,":",ice_factor_values[[pp]][ll])

          # Default level; terrible coding... xxx
          # Second row of dummy variable columns in main effects design are all 0s,
          # i.e. corresponds to default level
          if (ll == 1) {
            lab2 <- paste0(pp,":",ice_factor_values[[pp]][2])
            arrows( ll,
                    myem[[lab2]]$mean[ 2, paste0("y", yy) ] - 2 * myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                    ll,
                    myem[[lab2]]$mean[ 2, paste0("y", yy) ] + 2 * myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                    lwd = 1.2, angle = 90, length = 0.05, code = 3, col = col_darker)
            rect( ll - 0.1,
                  myem[[lab2]]$mean[ 2, paste0("y", yy) ] - myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                  ll + 0.1,
                  myem[[lab2]]$mean[ 2, paste0("y", yy) ] + myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                  col = "white", border = col_darker)
            lines( ll + 0.1*c(-1,1),
                   rep(myem[[lab2]]$mean[ 2, paste0("y", yy) ], 2), col = col_darker)


          } else { # Other levels

            stopifnot(lab %in% ice_dummy_list)

            arrows( ll,
                    myem[[lab]]$mean[ 1, paste0("y", yy) ] - 2 * myem[[lab]]$sd[ 1, paste0("y", yy) ],
                    ll,
                    myem[[lab]]$mean[ 1, paste0("y", yy) ] + 2 * myem[[lab]]$sd[ 1, paste0("y", yy) ],
                    lwd = 1.2, angle = 90, length = 0.05, code = 3, col = col_darker)
            rect( ll - 0.1,
                  myem[[lab]]$mean[ 1, paste0("y", yy) ] - myem[[lab]]$sd[ 1, paste0("y", yy) ],
                  ll + 0.1,
                  myem[[lab]]$mean[ 1, paste0("y", yy) ] + myem[[lab]]$sd[ 1, paste0("y", yy) ],
                  col = "white", border = col_darker)
            lines( ll + 0.1*c(-1,1),
                   rep(myem[[lab]]$mean[ 1, paste0("y", yy) ], 2), col = col_darker)

          }
        } # factor levels
      } # ice_factor_list

    } # Year loop

  } # if SA


}
