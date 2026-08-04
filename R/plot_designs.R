#' plot_designs: plot designs of ice simulations and emulator projections.
#'
#' @description Plot prior and posterior density estimates from Bayesian
#' calibration.
#'
#' @param data_type Data type: "sims" for simulations; "prior" or "posterior" for
#' emulator projections before or after calibration.
#' @param plot_level Plot level: 0 for none, 1 for main, 2 for exhaustive
#'
#' @export

# XXX Add legend to pairs plots using this old code
# text(8.8, 9.1, "SLE for RCP8.5", pos = 4, cex = 0.8)
# text(8.8, 8.6, "at 2100 (cm)", pos = 4, cex = 0.8)
# AddScale(breaks_SLE, sprintf("%.1f", breaks_SLE), colrng_SLE,
#         xlim = c(9, 9.5), ylim = c(4, 8), cex = 0.7 )

# Not writing to logfile because called from both build and main

plot_designs <- function(data_type, plot_level = 0) {

  par(cex.main = 0.7, cex.axis = 0.7, cex.lab = 0.7, mar = c(5, 4, 4, 2) + 0.1)

  # SIMS ------------------------------------------------------------

  # SIMULATIONS
  if (data_type == "sims") {

    # GSAT forcings ------------------------------------------------------------

    # GSAT
    if (plot_level >= 1) {

      # Plot GSAT anomalies w.r.t. 2015 for now
      to_plot <- climate_data[, 3:dim(climate_data)[2]] - climate_data[,"y2015"]
      to_plot_scen <- climate_data[, "scenario" ]

      # Add fixed climate forcings for GIS
      if ( i_s == "GIS" && final_year > 2100) {
        to_plot <- rbind( to_plot, climate_data_fixed[, 3:dim(climate_data_fixed)[2]] - climate_data_fixed[,"y2015"])
        to_plot_scen <- c(to_plot_scen, paste(climate_data_fixed[, "scenario" ], "fixed"))
      }

      plot(first_year:final_year, to_plot[1,], type = "n",
           ylim = range( to_plot, na.rm = TRUE ),
           xlim = c(first_year, final_year), xaxs = "i",
           xlab = "Year", ylab = "Global mean temperature relative to 2015 (degC)")
      abline(h=0, lwd=0.5)
      for( ss in 1:dim(to_plot)[1]) {
        col <- AR6_rgb[[ to_plot_scen[ ss ]  ]]
        if (is.null(col)) col <- AR6_rgb_med[[ strsplit(to_plot_scen[ ss ], " ")[[1]][1]  ]]
        if (is.null(col)) col <- "grey"
        lines( first_year:final_year, to_plot[ss, ], col = col, lwd = 1.2)
      }
      yleg <- 0.95

      for ( ff in sort(unique( to_plot_scen ) )) {

        # Look up nice name
        textlab <- scen_name[[ff]]
        if (is.null(textlab)) textlab <- paste(scen_name[[strsplit(ff, " ")[[1]][1] ]], "fixed")
        if (is.null(scen_name[[strsplit(ff, " ")[[1]][1] ]])) textlab <- ff # Or just use raw label

        # Same for colour
        col <- AR6_rgb[[ ff ]]
        if (is.null(col)) col <- AR6_rgb_med[[ strsplit( ff, " ")[[1]][1]  ]]
        if (is.null(col)) col <- "grey"

        text( first_year, yleg * max( to_plot, na.rm = TRUE ), pos = 4,
              textlab, col = col)
        yleg <- yleg - 0.07
      }

    } # plot level >= 1

    # Colour scale: maximum range of data
    if (min( ice_data[, paste0("y", c(cal_end:max(years_em))) ], na.rm = TRUE) < 0) {
      min_breaks <- 1.1 * min( ice_data[, paste0("y", c(cal_end:max(years_em))) ], na.rm = TRUE)
    } else min_breaks <- 0.9 * min( ice_data[, paste0("y", c(cal_end:max(years_em))) ], na.rm = TRUE)

    breaks_SLE <- seq( from = min_breaks,
                       to = 1.1*max( ice_data[, paste0("y", c(cal_end:max(years_em))) ], na.rm = TRUE),
                       length = 20 )
    #colrng_SLE <- pal(length(breaks_SLE) - 1)
    colrng_SLE <- hcl.colors(length(breaks_SLE) - 1, palette = "Blues")

    if (plot_level >= 2) {

      # Pairs: all continuous ------------------------------------------------------------
      # PLOT: PAIRS PLOTS OF CONTINUOUS PARAMETERS
      # print("Pairs plot: continuous model inputs")

      # All parameters
      for ( yy in yy_plot ) {

        col_em <- colrng_SLE[ cut( ice_data[, paste0("y", yy)  ], breaks = breaks_SLE) ]

        # These are the column indices for all ice model inputs in the CSV (see main.R)
        # i.e. between "ice_source", "region", "group", "model","scenario"
        # and y_1,..., y_last
        #param_cols <- ice_param_col_1:ice_param_col_2

        # Exclude column of NAs
        # xxx YUK!! change to col name at least, or do NA check as below for factor plots
        # -> added a check
        # if ( length(model_list) == 1 && model_list == "OGGM" ) param_cols <- param_cols [-2]

        # See below for bits of starting code...
        # Exclude column if no non-missing values exist
        # xxx probably a better way...maybe in pairs()?
        #for ( pp in start_col:end_col) {
        #  if ( length(ice_data[ !is.na(ice_data[ , pp ]), pp ]) == 0 ) {
        #   param_cols <- param_cols[-pp]
        #}
        # na.omit??

        # Get continuous ice model inputs
        cols_to_plot <- cbind(temps, ice_data[, ice_cont_list])
        colnames(cols_to_plot)[1:length(temps_list_names)] <- temps_list_names

        if (ncol(cols_to_plot) < 15) {
          pairs( cols_to_plot, main = paste("Sea level contribution in",yy,"vs continuous inputs"),
                 lower.panel = NULL, pch = 20, cex = 1.5, col = col_em)
        }
      }

    } # all plots

    # * Pairs: emulator inputs ------------------------------------------------------------
    # PLOT: pairs plot for just the parameters in the emulator
    # print("Pairs plot: continuous model inputs used by emulator")

    for ( yy in yy_plot ) {

      if (ncol(ice_design) < 15) {

        col_em <- colrng_SLE[ cut( ice_data[, paste0("y", yy)  ], breaks = breaks_SLE) ]

        pairs( ice_design,
               main = paste("Sea level contribution in",yy,"vs all inputs"),
               lower.panel = NULL, pch = 20, cex = 1.5, col = col_em)
      }
    }

    # 2x1 plots from here
    par(mfrow = c(1,2), pin = c(2.4,2.4) )

    # * SLE vs GSAT ------------------------------------------------------------
    # print("Scatter plot: sea level contribution vs climate inputs")

    # PLOT: SIMULATED SLE VS EMULATOR INPUTS
    for ( yy in yy_plot ) {

      # PLOT: GSAT
      for ( tt in 1:length(temps_list)) {

        if (length(temps_list) == 1) {
          plot_temps <- temps
          temp_name <- colnames(ice_design)[1]
        } else {
          plot_temps <- temps[ , tt ]
          temp_name <- colnames(temps)[tt]
        }

        plot( plot_temps, ice_data[ , paste0("y", yy) ], type = "n",
              main = paste("Sea level contribution in",yy,"vs",temp_name),
              xlab = GSAT_lab[[temps_list_names[tt]]],
              ylab = paste("Sea level contribution at",yy,"(cm SLE)") )

        # PLOT OBSERVATIONS # xxx not plotting? [fixed now I think]
        if (yy == cal_end) {
          abline( h = obs_change, col = grey(0.2, 0.4), lwd = 1.6)
          rect( min(plot_temps, na.rm = TRUE), obs_change - 3 * obs_err,
                max(plot_temps, na.rm = TRUE), obs_change + 3 * obs_err,
                col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
          if (plot_level > 2) {
            rect( min(plot_temps, na.rm = TRUE), obs_change - 3 * tot_err,
                  max(plot_temps, na.rm = TRUE), obs_change + 3 * tot_err,
                  col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
          }
        } # plot obs

        leg_y <- 0.95 * max( ice_data[ , paste0("y", yy) ], na.rm = TRUE)

        # Plot simulations
        # xxx not plotting when called from main.R
        for (scen in scenario_list) {
          if ( length(ice_data[ ice_data$scenario == scen, paste0("y", yy) ]) > 0 ) {
            points( plot_temps[ice_data$scenario == scen],
                    ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                    pch = 20, cex = 0.8, col = AR6_rgb[[scen]] )
            # Legend
            text( min(plot_temps, na.rm = TRUE), leg_y, cex = 0.5,
                  scen_name[[scen]], pos = 4, col = AR6_rgb[[scen]] )
            leg_y <- leg_y - 0.05 * leg_y
          }
        }

      }
    }

    #if (plot_level >= 2) {

    # (*) SLE vs all inputs ------------------------------------------------------------
    # PLOT: SIMULATED SLE VS EMULATOR/ALL INPUTS
    # Box plots

    # Loop over full input list in data file, not just inputs selected for emulation
    for ( pp in ice_param_list_full) {

      # If continuous ice input or plot all inputs
      if (pp %in% ice_cont_list || plot_level >= 2) {

        # If non-missing values exist in column
        if ( length(ice_data[ !is.na(ice_data[ , pp ]), pp ]) > 0 ) {

          # Deal with factor inputs
          if (is.character(ice_data[ 1, pp ])) { param_plot <- factor(ice_data[ , pp ])
          } else param_plot <- ice_data[ , pp ]

          for (yy in yy_plot ) {

            plot( param_plot, ice_data[ , paste0("y", yy) ],
                  main = paste0( "Sea level contribution in ",yy," vs ", pp),
                  xlab = pp, cex = 0.8,
                  ylab = paste("Sea level contribution at",yy,"(cm SLE)") )

            # Overplot continuous inputs in scenario colours
            # xxx this is failing when calling from main.R
            if (! is.character(ice_data[ 1, pp ])){

              leg_y <- 0.95 * max(ice_data[ , paste0("y", yy) ], na.rm = TRUE)

              for (scen in scenario_list) {
                if ( length(ice_data[ ice_data$scenario == scen, paste0("y", yy) ]) > 0 ) {
                  points( param_plot[ice_data$scenario == scen],
                          ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                          pch = 20, col = AR6_rgb[[scen]], cex = 0.8 )
                  # Legend
                  text( min(param_plot, na.rm = TRUE), leg_y, cex = 0.5,
                        scen_name[[scen]], pos = 4, col = AR6_rgb[[scen]] )
                  leg_y <- leg_y - 0.05 * leg_y
                }
              }

              # PLOT OBSERVATIONS
              if (yy == cal_end) {
                abline( h = obs_change, col = grey(0.2, 0.4), lwd = 1.6)
                rect( min(param_plot, na.rm = TRUE), obs_change - 3 * obs_err,
                      max(param_plot, na.rm = TRUE), obs_change + 3 * obs_err,
                      col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)
                if (plot_level > 2) {
                  rect( min(param_plot, na.rm = TRUE), obs_change - 3 * tot_err,
                        max(param_plot, na.rm = TRUE), obs_change + 3 * tot_err,
                        col = grey(0.2,0.03), border = "black", lwd = 0.5, lty = 3)
                }
              } # plot obs
            } # continuous
          } # years
        } # non-missing
      } # plot level = 2 or emulator input
    } # parameter loop



    if (FALSE) {
      # print("Scatter plot: sea level contribution vs model inputs used by emulator")

      # PLOT: SIMULATED SLE VS CONTINUOUS INPUTS USED FOR EMULATOR
      for (yy in yy_plot ) {

        for (pp in ice_cont_list) {

          plot( unlist(ice_design[,pp]), ice_data[ , paste0("y", yy) ], type = "n",
                main = paste0("Sea level contribution at ",yy," vs ", pp), xlab = pp,
                ylab = paste("Sea level contribution at",yy,"(cm SLE)") )

          leg_y <- 0.9 * max( ice_data[ , paste0("y", yy) ], na.rm = TRUE)

          for (scen in scenario_list) {
            if ( length(ice_data[ ice_data$scenario == scen, paste0("y", yy) ]) > 0 ) {

              points( unlist(ice_design[,pp])[ ice_data$scenario == scen ],
                      ice_data[ ice_data$scenario == scen, paste0("y", yy) ],
                      pch = 20, cex = 0.8, col = AR6_rgb[[scen]] )
              text( min(unlist(ice_design[,pp]), na.rm = TRUE), leg_y, cex = 0.7,
                    scen, pos = 4, col = AR6_rgb[[scen]] )
              leg_y <- leg_y - 0.1 * leg_y
            }
          }
        } # params
      }

    } # FALSE

    if (plot_level >= 2) {

      # Histogram: GSAT ------------------------------------------------------------
      # PLOT: GSAT of simulations

      for ( tt in 1:length(temps_list)) {
        if (length(temps_list) == 1) { plot_temps <- temps
        } else plot_temps <- temps[, tt]

        hist(plot_temps, col = "darkgrey", breaks = 40,
             main = paste0("Ensemble distribution: ",temps_list_names[tt]),
             xlab = GSAT_lab[[temps_list_names[tt]]] )
      }

      # Histogram: ice inputs ------------------------------------------------------------

      # PLOT: ice model parameter(s) xxx currently just continuous
      for (pp in ice_cont_list) {
        hist(unlist(ice_design[,pp]), col = "darkgrey", breaks = 40, # cex.axis = 1.5, cex.lab = 1.2,
             main = paste0("Ensemble distribution: ",pp), xlab = pp)
      }
    } # plot level

  } # sims

  # EMULATOR ------------------------------------------------------------

  # EMULATOR PROJECTIONS
  if (plot_level >= 2) {

    # 2x1 again
    par(mfrow = c(1,2), pin = c(2.4,2.4), cex.main = 0.7, cex.axis = 0.7, cex.lab = 0.7)

    # Pairs: mean projections vs inputs ------------------------------------------------------------
    # print("Pairs: mean projections vs inputs")

    # PLOT: pairs plot of emulator projections, i.e. priors on inputs
    if (data_type == "prior") {

      for (scen in scenario_list) {

        # Colour scale: using full range of projections (assumes max is positive)
        if ( min(myem[[scen]]$mean, na.rm = TRUE) < 0 ) {
          min_breaks <- 1.1*min(myem[[scen]]$mean, na.rm = TRUE)
        } else min_breaks <- 0.9*min(myem[[scen]]$mean, na.rm = TRUE)
        breaks_SLE <- seq( from = min_breaks,
                           to = 1.1*max(myem[[scen]]$mean, na.rm = TRUE),
                           length = 20 )
        colrng_SLE <- hcl.colors(length(breaks_SLE) - 1, palette = "Blues")

        for ( yy in yy_plot ) {

          # Gets silly when many factor levels
          if (ncol(design_pred[[scen]]) < 15) {

            col_em <- colrng_SLE[ cut(myem[[scen]]$mean[ , paste0("y",yy) ],
                                      breaks = breaks_SLE) ]
            pairs(design_pred[[scen]],
                  main = paste0("Mean emulator projections in ", yy, " for ", scen_name[[scen]], " vs inputs" ),
                  lower.panel = NULL, pch = 20, col = col_em)
          }
        } # year loop
      } # prior
    } # scenarios

    # Prior: GSAT ------------------------------------------------------------
    # PLOT: GSAT priors

    for (scen in scenario_list) {

      if (data_type == "prior") {

        # PLOT: GSAT
        for ( tt in 1:length(temps_list)) {
          hist( design_prior[[scen]][ , tt],
                main = paste("Prior:", temps_list_names[tt], "for", scen_name[[scen]]),
                xlab = GSAT_lab[[temps_list_names[tt]]],
                xlim = c(0,14),
                breaks = seq(from = -5, to = 20, by = 0.25),
                cex.axis = 0.8, cex.lab = 0.8, col = AR6_rgb[[scen]] )
        }

      }

      if (data_type == "posterior") {

        # PLOT: GSAT
        for ( tt in 1:length(temps_list)) {
          hist( design_pred[[scen]][ , tt],
                main = paste("Posterior:", temps_list_names[tt], "for", scen_name[[scen]]),
                xlab = GSAT_lab[[temps_list_names[tt]]],
                xlim = c(0,14),
                breaks = seq(from = -5, to = 20, by = 0.25),
                cex.axis = 0.8, cex.lab = 0.8, col = AR6_rgb[[scen]] )
        }
      }
    } # scenario

    # Prior: ice model inputs ------------------------------------------------------------
    # PLOT: ice parameters

    # 7/1/25 move this down to design_prior plot and add posterior flag to next
    # if (data_type == "prior") {

    # PLOT: ice sheet model parameter prior
    for (scen in scenario_list) {

      if (data_type == "prior") {
        for (pp in ice_all_list ) {
          hist(design_prior[[scen]][,pp],
               main = paste("Prior:", pp, "for", scen_name[[scen]]),
               xlab = pp, col = "cornflowerblue", breaks = 40,
               cex.axis = 1.5, cex.lab = 1.2 )
        }
      }

      if (data_type == "posterior") {
        for (pp in ice_all_list ) {
          hist(design_pred[[scen]][,pp],
               main = paste("Posterior:", pp, "for", scen_name[[scen]]),
               xlab = pp, col = "cornflowerblue", breaks = 40,
               cex.axis = 1.5, cex.lab = 1.2 )
        }
      }
    }

    #  } # prior

  } # plot_level >= 2

} # plot_designs
