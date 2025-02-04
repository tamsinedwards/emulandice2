#' plot_distributions: plot distributions.
#'
#' @description Plot distributions (histograms and density estimates). See
#' also plot_Bayesian.R.
#'
#' @param data_type Data type: "sims" for simulations; "prior" or "posterior" for
#' emulator projections before or after calibration.
#' @param plot_level Plot level: 0 for none, 1 for main, 2 for exhaustive

#' @export

plot_distributions <- function(data_type, plot_level = 0) {

  # PROJECTION HISTOGRAMS AND PDFS

  cfb_pale <- rgb(100,149,237, alpha = 50, maxColorValue = 255)
  cfb_med <- rgb(100,149,237, alpha = 100, maxColorValue = 255)
  cfb_dark <- rgb(100,149,237, alpha = 150, maxColorValue = 255)

  # Mean projections
  # xxx could loop around mean, final, and Bayesian sample with same code
  if (plot_level >= 2) {

    for (yy in yy_plot ) {

      for (scen in scenario_list) {

        # PRIOR ----------------------------------------------------------
        if (data_type == "prior") {


          # Get x breaks and y scale
          proj_breaks <- seq( from = floor(min(myem[[scen]]$mean[ , paste0("y",yy) ])),
                              to = ceiling(max(myem[[scen]]$mean[ , paste0("y",yy) ])),
                              length = 50)

          # Get y axis
          post_hist <- hist( myem[[scen]]$mean[ , paste0("y",yy) ], plot = FALSE,
                             breaks = proj_breaks )

          # Prior mean ------------------------------------------------------------

          # Mean projections
          hist( myem[[scen]]$mean[ , paste0("y",yy) ],
                ylim = c(0,max(post_hist$density)),
                main = paste("Mean prior projections:", scen_name[[scen]]),
                freq = FALSE, xlim = sle_lim[[yy]],
                breaks = proj_breaks,
                col = cfb_pale, border = NA,
                xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines( density(myem[[scen]]$mean[,paste0("y",yy)]),
                 col = cfb_med, lwd = 1.2)

        } # prior

      } # scenario loop

    } #years
  } # all plots

  if (plot_level >= 1) {

    for (yy in yy_plot ) {

      for (scen in scenario_list) {


        if (plot_level >= 2 && data_type == "prior") {


          # Get x breaks and y scale
          proj_breaks <- seq( from = floor(min(projections[[scen]][ , paste0("y",yy) ])),
                              to = ceiling(max(projections[[scen]][ , paste0("y",yy) ])),
                              length = 50)

          # Get y axis
          post_hist <- hist( projections[[scen]][ , paste0("y",yy) ], plot = FALSE,
                             breaks = proj_breaks )

          # Prior final ------------------------------------------------------------

          # Full projections
          hist(projections[[scen]][ , paste0("y",yy) ],
               ylim = c(0,max(post_hist$density)),
               main = paste( "Final prior projections:", scen_name[[scen]] ),
               freq = FALSE, xlim = sle_lim[[yy]],
               breaks = proj_breaks,
               col = cfb_pale, border = NA,
               xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines(density(projections[[scen]][,paste0("y",yy)]),
                col = cfb_med, lwd = 1.2)

        }

        # POSTERIOR ----------------------------------------------------------
        if (data_type == "posterior") {

          # Get x breaks and y scale from prior & posterior
          proj_breaks <- seq( from = floor(min( c(projections[[scen]][ , paste0("y",yy) ],
                                                  proj_post[[scen]][ , paste0("y",yy) ]) )),
                              to = ceiling(max( c(projections[[scen]][ , paste0("y",yy) ],
                                                  proj_post[[scen]][ , paste0("y",yy) ]) )),
                              length = 50)

          # Get y axis from posterior
          post_hist <- hist(proj_post[[scen]][ , paste0("y",yy) ], plot = FALSE,
                            breaks = proj_breaks)

          # Posterior final ----------------------------------------------------

          # Start by plotting prior final to compare
          hist(projections[[scen]][ , paste0("y",yy) ],
               ylim = c(0,max(post_hist$density)),
               main = paste( "Prior and posterior final projections", scen_name[[scen]] ),
               freq = FALSE, xlim = sle_lim[[yy]],
               breaks = proj_breaks,
               col = cfb_pale, border = NA,
               xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines(density(projections[[scen]][, paste0("y",yy)]),
                col = cfb_med, lwd = 1.2)

          # Full projections: Bayesian sample (final FACTS projections)
          hist(proj_post[[scen]][ , paste0("y",yy) ],
               freq = FALSE, breaks = proj_breaks,
               col = cfb_med, border = NA, add = TRUE)
          if (length(proj_post[[scen]][ , paste0("y",yy) ]) > 2) {
            lines(density(proj_post[[scen]][ , paste0("y",yy)]),
                  col = "darkblue", lwd = 1.2)
          }

        } # posterior


      } # end scenario loop

    } # years

  } # plot level

} # END FUNCTION
