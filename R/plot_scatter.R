#' plot_scatter: plot scatter (X-Y) figures.
#'
#' @description Plot scatter and X-Y plots, including main effects.
#'
#' @param data_type Data type: "sims" for simulations; "prior" or "posterior" for
#' emulator projections before or after calibration.
#' @param design_name Some plots are for main effects or projections only; some only for sims
#' Takes values none (sims); unif_temps or AR6_2LM (projections)
#' @param plot_level Plot level: 0 for none, 1 for main, 2 for exhaustive

#' @export

# Not writing to logfile because called from both build and main
plot_scatter <- function(data_type, design_name, plot_level = 0) {

  # Designs: simulations, SA and prediction
  # Design unif_temps is used by emulator_build.R for emulator SA i.e. validation,
  # AR6_2LM is used by main.R for main predictions
  stopifnot(design_name %in% c("none", "unif_temps", "AR6_2LM", "validation"))

  # Data: simulations (design = "none"),
  # uncalibrated emulator ("unif_temps" or "AR6_2LM"), or Bayesian calibrated emulator ("AR6_2LM")
  if (design_name == "none") stopifnot(data_type == "sims")
  if (design_name %in% c("unif_temps", "AR6_2LM")) stopifnot(data_type %in% c("prior", "posterior"))

  # Add test dataset validation option
  if (design_name == "validation") stopifnot(data_type == "tvt")
  if (data_type == "tvt") stopifnot(design_name == "validation")

  par(mfrow = c(1,2), pin = c(2.7,2.7), cex.main = 0.6, cex.axis = 0.7, cex.lab = 0.7)

  for (scen in scenario_list) {

    # Get equivalent RCPs or other names for this scenario (see scen_match in plotting_functions.R)
    match <- scen_match(scen)

    for (yy in yy_plot ) {

      if (yy == cal_end) next # because plotting vs cal_end

      # Get xrange limit for these
      xlim <- sle_lim[[as.character(cal_end)]]

      # * Future vs past: mean ------------------------------------------------------------
      # Future vs past for simulations or SA/prediction emulator designs

      if ( data_type %in% c("sims", "prior", "tvt") ) {

        # PLOT CALIBRATION SCATTER: FUTURE VS PAST - mean [ option: +/- 3 s.d. error bars ]
        plot(1:3, 1:3, type = "n",
             main = paste0( ice_name, " ", yy, " vs ", cal_end,": ", scen_name[[scen]],
                            " mean"), # TODO: fix name for when sims only
             xlim = xlim, ylim = sle_lim[[yy]], xaxs = "i", yaxs = "i",
             cex.main = 0.7,
             xlab = paste("Sea level contribution from",cal_start,"-",cal_end,"(cm)"),
             ylab = paste("Sea level contribution from",cal_start,"-",yy,"(cm)"))
        abline( h = 0 )
        if (i_s == "GLA") {
          abline( h = glacier_cap, col = "darkred", lwd = 0.5, lty = 5)
        }

        # Plot observations
        abline( v = obs_change,
                col = grey(0.2, 0.4), lwd = 1.6)
        rect( obs_change - 3 * obs_err, sle_lim[[yy]][1],
              obs_change + 3 * obs_err, sle_lim[[yy]][2],
              col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
        if (plot_level > 2) {
          rect( obs_change - 3 * tot_err, sle_lim[[yy]][1],
                obs_change + 3 * tot_err, sle_lim[[yy]][2],
                col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
        }

        if (data_type %in% c("prior", "tvt")) {

          # Emulator prediction design to plot: prior or test data design used in validation
          if (data_type == "prior") {
            emu_mean <- myem[[scen]]$mean
            emu_sd <- myem[[scen]]$sd
          }
          if (data_type == "tvt") {

            # Get test dataset rows for scenario(s)
            ii <- which(ice_data$scenario[test_set] %in% match)

            # Get emulator predictions for these simulations
            emu_mean <- emu_test$mean[ii, , drop = FALSE]
            emu_sd   <- emu_test$sd[ii, , drop = FALSE]
          }

          # Plot emulated
          if (data_type == "prior" || (data_type == "tvt" && length(ii) > 0) ) {

            # Mean
            points(emu_mean[ , paste0("y",cal_end) ],
                   emu_mean[ , paste0("y", yy) ], cex = 0.7,
                   pch = 16, col = AR6_rgb_light[[scen]])

            # Horizontal error bars
            arrows( emu_mean[ , paste0("y",cal_end) ] - 2 * emu_sd[ , paste0("y",cal_end) ],
                    emu_mean[ , paste0("y", yy) ],
                    emu_mean[ , paste0("y",cal_end) ] + 2* emu_sd[ , paste0("y",cal_end) ],
                    emu_mean[ , paste0("y", yy) ],
                    code = 3, length = 0.06, angle = 90, lwd = 0.1,
                    col = AR6_rgb_light[[scen]])

            # Vertical error bars
            arrows( emu_mean[ , paste0("y",cal_end) ],
                    emu_mean[ , paste0("y", yy) ] - 2 * emu_sd[ , paste0("y", yy) ],
                    emu_mean[ , paste0("y",cal_end) ],
                    emu_mean[ , paste0("y", yy) ] + 2 * emu_sd[ , paste0("y", yy) ],
                    code = 3, length = 0.06, angle = 90, lwd = 0.1,
                    col = AR6_rgb_light[[scen]])

            # Legend
            yleg <- 0.90 * sle_lim[[yy]][2]
            points( xlim[1] + 0.05*(xlim[2] - xlim[1]), yleg, pch = 16, col = AR6_rgb_med[[scen]], cex = 0.7)
            text(x = xlim[1] + 0.05*(xlim[2] - xlim[1]), y = yleg, pos = 4, "Emulated mean +/- 2 s.d.", cex = 0.7)

          }

        } # prior or tvt

        # ADD SIMULATED IN BLACK
        yleg <- 0.82*sle_lim[[yy]][2]

        # Get comparison simulations for this scenario
        if (data_type == "tvt") { plot_data <- test_data[ ii, , drop = FALSE]
        } else plot_data <- ice_data[ ice_data$scenario %in% match, , drop = FALSE]

        # Plot simulations, and add legend for SSP
        # TODO: add equivalent RCP names to legend
        if ( nrow(plot_data) > 0 ) {
          apply( plot_data, 1,
                 function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y",yy) ],
                                     pch = 16, cex = 0.5, col = "black" ) )
          points( xlim[1] + 0.05*(xlim[2] - xlim[1]), yleg,
                  pch = 16, cex = 0.7, col = "black" )
          text(x = xlim[1] + 0.05*(xlim[2] - xlim[1]), y = yleg, pos = 4, "Simulated", cex = 0.7)

        }

      }  # if data_type sims or prior

      # * Future vs past: final ------------------------------------------------------------

      #___________________________________________________________________________
      # PLOT CALIBRATION SCATTER: FUTURE VS PAST - final
      # Same again but for emulator posterior

      # Posterior
      if (data_type == "posterior") {

        stopifnot( design_name %in% c("unif_temps","AR6_2LM") ) # not sims as would duplicate above

        plot(1:3, 1:3, type = "n",
             main = paste0( ice_name, " ", yy, " vs ", cal_end,": ", scen_name[[scen]], " final"),
             xlim = xlim, ylim = sle_lim[[yy]], xaxs = "i", yaxs = "i",
             xlab = paste("Sea level contribution from",cal_start,"-",cal_end,"(cm)"),
             ylab = paste("Sea level contribution from",cal_start,"-",yy,"(cm)"))
        abline( h = 0, lwd = 0.2, col = "darkgrey" )
        if (i_s == "GLA") {
          abline( h = glacier_cap, col = "darkred", lwd = 0.5, lty = 5)
        }

        # Observations
        abline( v = obs_change, col = grey(0.2, 0.4), lwd = 1.6)
        rect( obs_change - 3 * obs_err, sle_lim[[yy]][1],
              obs_change + 3 * obs_err, sle_lim[[yy]][2],
              col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
        if (plot_level > 2) {
          rect( obs_change - 3 * tot_err, sle_lim[[yy]][1],
                obs_change + 3 * tot_err, sle_lim[[yy]][2],
                col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
        }
        # EMULATED
        points(projections[[scen]][ , paste0("y",cal_end) ],
               projections[[scen]][ , paste0("y",yy) ], pch = 16, cex = 0.5,
               col = AR6_rgb_light[[scen]], bg = AR6_rgb_light[[scen]])

        yleg <- sle_lim[[yy]][1] + 0.87*(sle_lim[[yy]][2] - sle_lim[[yy]][1])
        points( xlim[1] + 0.05*(xlim[2] - xlim[1]), yleg, pch = 16, cex = 0.7,
                col = AR6_rgb_light[[scen]], bg = AR6_rgb_light[[scen]] )
        text(x = xlim[1] + 0.05*(xlim[2] - xlim[1]), y = yleg, pos = 4, "Emulated", cex = 0.7)

        # SIMULATIONS
        yleg <- sle_lim[[yy]][1] + 0.92*(sle_lim[[yy]][2] - sle_lim[[yy]][1])

        # Get simulations for this scenario
        #        plot_data <- ice_data[ ice_data$scenario == scen, ]

        # Add nearest RCPs
        # TODO: add RCP names to legend
        #        if (scen == "SSP126") plot_data <- ice_data[ ice_data$scenario %in% c("RCP26", "SSP126"), ]
        #        if (scen == "SSP245") plot_data <- ice_data[ ice_data$scenario %in% c("RCP45", "SSP245"), ]
        #        if (scen == "SSP534-over") plot_data <- ice_data[ ice_data$scenario %in%  c("SSP534-over", "SSP534-over-recon"), ]
        #        if (scen == "SSP585") plot_data <- ice_data[ ice_data$scenario %in% c("RCP85", "SSP585"), ]

        # Get comparison simulations for this scenario
        # (No tvt option for this plot)
        plot_data <- ice_data[ ice_data$scenario %in% match, , drop = FALSE]

        if (nrow(plot_data) > 0) {
          apply( plot_data, 1,
                 function(x) points( x[ paste0("y",cal_end) ], x[ paste0("y",yy) ],
                                     pch = 16, cex = 0.5, col = "black" ) )
          points( xlim[1] + 0.05*(xlim[2] - xlim[1]), yleg,
                  pch = 16, cex = 0.7, col = "black" )
          text(x = xlim[1] + 0.05*(xlim[2] - xlim[1]), y = yleg, pos = 4, "Simulated", cex = 0.7)

        }

      } # posterior
    }  # year list yy_plot
  } # scenario_list

  if (plot_level >= 2) {

    for (scen in scenario_list) {

      # Get equivalent RCPs or other names for this scenario (see scen_match in plotting_functions.R)
      match <- scen_match(scen)

      # EMULATOR

      # NOTE THIS SHOULD REALLY BE CAL_END - CAL_START
      # BUT WORKS BECAUSE CAL_START IS ALWAYS ZERO FOR NOW
      # [does this refer to yy or plot?]
      for (yy in yy_plot) {

        # SEA LEVEL VS TEMP TIMESLICES
        # Mean projections
        for (gg in temps_list_names) {

          # SLE vs GSAT: mean ------------------------------------------------------------
          if (data_type %in% c("prior", "tvt")) {

            # Emulator prediction design to plot: prior or test data design used in validation
            if (data_type == "prior") {
              emu_mean <- myem[[scen]]$mean
              emu_sd <- myem[[scen]]$sd
              emu_gg <- design_pred[[scen]][ , gg, drop = FALSE]
            }
            if (data_type == "tvt") {

              # Get test dataset rows for scenario(s)
              ii <- which(ice_data$scenario[test_set] %in% match)

              # Get emulator predictions for these simulations
              emu_mean <- emu_test$mean[ii, , drop = FALSE]
              emu_sd   <- emu_test$sd[ii, , drop = FALSE]
              emu_gg <- ice_design[test_set[ii], gg, drop = FALSE]
            }

            # Guard against no test data for validation plots
            if (data_type == "prior" || (data_type == "tvt" && length(ii) > 0) ) {

              # Plot emulated: mean
              plot( emu_gg, emu_mean[,paste0("y",yy)],
                    pch = 16, col = AR6_rgb_light[[scen]], cex = 0.7,
                    main = paste( "Mean projections at",yy,"for", scen_name[[scen]] ),
                    xlab = GSAT_lab[[gg]],
                    ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                    ylim = sle_lim[[yy]])
              abline( h = 0 )

              # TODO: minor fix for x limits of rect (extend past design limits)
              if (yy == cal_end) {
                abline( h = obs_change,
                        col = grey(0.2, 0.4), lwd = 1.6)
                rect( min(emu_gg), obs_change - 3 * obs_err,
                      max(emu_gg), obs_change + 3 * obs_err,
                      col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
                if (plot_level > 2) {
                  rect( min(emu_gg), obs_change - 3 * tot_err,
                        max(emu_gg), obs_change + 3 * tot_err,
                        col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
                }
              }

              # Plot emulated: error bars
              arrows( emu_gg, emu_mean[ , paste0("y", yy) ] - 2 * emu_sd[ , paste0("y", yy) ],
                      emu_gg, emu_mean[ , paste0("y", yy) ] + 2 * emu_sd[ , paste0("y", yy) ],
                      code = 3, length = 0.06, angle = 90, lwd = 0.1,
                      col = AR6_rgb_light[[scen]])

              # Get comparison simulations for this scenario
              if (data_type == "tvt") {
                plot_data <- test_data[ ii, , drop = FALSE]
                plot_design <- emu_gg # already selected gg columns(s) because inside gg loop
              } else {
                plot_data <- ice_data[ ice_data$scenario %in% match, , drop = FALSE]
                if (length(temps_list) == 1) { plot_design <- temps[ice_data$scenario %in% match]
                } else plot_design <- temps[ice_data$scenario %in% match, gg, drop = FALSE]
              }

              points( plot_design, plot_data[ , paste0("y", yy) ], pch = 16, cex = 0.7)

            } # if prior or test data
          } # if prior or tvt

          # SLE vs GSAT: full ------------------------------------------------------------
          # Full projections
          if (data_type == "posterior") {

            plot( design_pred[[scen]][,gg], projections[[scen]][,paste0("y",yy)],
                  pch = 16, col = AR6_rgb_light[[scen]], cex = 0.7,
                  main = paste( "Final projections at",yy,"for", scen_name[[scen]] ),
                  xlab = GSAT_lab[[gg]],
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  ylim = sle_lim[[yy]])
            abline( h = 0 )

            if (yy == cal_end) {
              abline( h = obs_change,
                      col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(design_pred[[scen]][,gg]),
                    obs_change - 3 * obs_err,
                    max(design_pred[[scen]][,gg]),
                    obs_change + 3 * obs_err,
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(design_pred[[scen]][,gg]), obs_change - 3 * tot_err,
                      max(design_pred[[scen]][,gg]), obs_change + 3 * tot_err,
                      col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }

            # TODO: add RCPs! and add names to legend
            if (length(temps_list) == 1) {
              points( temps[ice_data$scenario == scen], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                      pch = 16, cex = 0.7) # col = AR6_rgb[[scen]],
            } else points( temps[ice_data$scenario == scen, gg], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                           pch = 16, cex = 0.7) # col = AR6_rgb[[scen]],

          } # if posterior

        } # GSAT loop

        # SLE vs ice inputs: mean ------------------------------------------------------------
        if (data_type %in% c("prior", "tvt")) {

          # Emulator prediction design to plot: prior or test data design used in validation
          if (data_type == "prior") {
            emu_mean <- myem[[scen]]$mean
            emu_sd <- myem[[scen]]$sd
            emu_pp <- design_pred[[scen]]
          }
          if (data_type == "tvt") {

            # Get test dataset rows for scenario(s)
            ii <- which(ice_data$scenario[test_set] %in% match)

            # Get emulator predictions for these simulations
            emu_mean <- emu_test$mean[ii, , drop = FALSE]
            emu_sd <- emu_test$sd[ii, , drop = FALSE]
            emu_pp <- ice_design[test_set[ii], , drop = FALSE]
          }

          # Guard against no test data for validation plots
          if (data_type == "prior" || (data_type == "tvt" && length(ii) > 0) ) {

          # SEA LEVEL VS ICE MODEL PARAMETER
          # Plot mean and full projections vs each parameter in turn
          for (pp in ice_all_list) {

            # Plot emulated: mean
            plot( emu_pp[ , pp], emu_mean[ , paste0("y",yy)],
                  pch = 16, col = AR6_rgb_light[[scen]], cex = 0.7,
                  main = paste("Mean projections at",yy,"for", scen_name[[scen]]),
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  xlab = pp, ylim = sle_lim[[yy]])
            abline( h = 0 )

            if (yy == cal_end) {
              abline( h = obs_change, col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(emu_pp[ , pp ]), obs_change - 3 * obs_err,
                    max(emu_pp[ , pp ]), obs_change + 3 * obs_err,
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(emu_pp[ , pp ]), obs_change - 3 * tot_err,
                      max(emu_pp[ , pp ]), obs_change + 3 * tot_err,
                      col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }

            # Plot emulated: error bars
            if (data_type == "prior" || (data_type == "tvt" && length(ii) > 0) ) {

              arrows( emu_pp[ , pp ], emu_mean[ , paste0("y", yy) ] - 2 * emu_sd[ , paste0("y", yy) ],
                      emu_pp[ , pp ], emu_mean[ , paste0("y", yy) ] + 2 * emu_sd[ , paste0("y", yy) ],
                      code = 3, length = 0.06, angle = 90, lwd = 0.1,
                      col = AR6_rgb_light[[scen]])
            }

            # Get comparison simulations for this scenario
            if (data_type == "tvt") {
              plot_data <- test_data[ ii, , drop = FALSE]
              plot_design <- emu_pp[, pp]
            } else {
              plot_data <- ice_data[ ice_data$scenario %in% match, , drop = FALSE]
              plot_design <- ice_design[ ice_data$scenario %in% match, pp, drop = FALSE ] # was unlist(ice_design[,pp])[ ice_data$scenario %in% match,  ]
            }

            # Plot simulations
            points( plot_design, plot_data[ , paste0("y", yy) ], pch = 16, cex = 0.7)

          } # param list

          } # if prior or test data

        } # if prior or tvt

        # SLE vs ice inputs: final ------------------------------------------------------------
        if (data_type == "posterior") {

          for (pp in ice_all_list) {

            # SAME AGAIN BUT FINAL PROJECTIONS

            plot( design_pred[[scen]][,pp], projections[[scen]][,paste0("y",yy)],
                  pch = 16, cex = 0.7, col = AR6_rgb_light[[scen]],
                  main = paste("Final projections at",yy,"for", scen_name[[scen]]),
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
                  xlab = pp, ylim = sle_lim[[yy]] )
            abline( h = 0 )

            # TODO: add RCPs! and add names to legend
            points( unlist(ice_design[,pp])[ice_data$scenario == scen], ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                    pch = 16, cex = 0.7 ) # col = AR6_rgb[[scen]],

            # ADD OBSERVATIONS
            if (yy == cal_end) {
              abline( h = obs_change,
                      col = grey(0.2, 0.4), lwd = 1.6)
              rect( min(design_pred[[scen]][,pp]), obs_change - 3 * obs_err,
                    max(design_pred[[scen]][,pp]), obs_change + 3 * obs_err,
                    col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
              if (plot_level > 2) {
                rect( min(design_pred[[scen]][,pp]), obs_change - 3 * tot_err,
                      max(design_pred[[scen]][,pp]), obs_change + 3 * tot_err,
                      col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
              }
            }
          } # param list

        } # if posterior

      } # Year
    } # SSP

  } # plot_level >= 2


}
