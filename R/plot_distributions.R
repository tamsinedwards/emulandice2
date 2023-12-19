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

  # Some rough axis limits
  #  ylim_dist <- c(-60,90)
  #  if (i_s == "GLA") {
  #    ylim_dist <- c(-5,15)
  #    if (reg %in% c("RGI12", "RGI18")) ylim_dist <- c(-0.005,0.03)
  #  }

  xlim_dist <- ylim_max # c(-30,80)  # -2, 7
  # if (i_s == "GLA") {
  #    xlim_dist <- c(-2,10)
  #    if (reg %in% c("RGI12", "RGI18")) xlim_dist <- c(-0.005,0.03)
  #  }


  # Distributions ------------------------------------------------------------

  # PROJECTION HISTOGRAMS AND PDFS

  # Mean projections
  if (plot_level >= 2) {

    for (yy in yy_plot ) {

      for (scen in scenario_list) {

        if (data_type == "posterior") {

          hist(myem[[scen]]$mean[ , paste0("y",yy) ],
               main = paste("Uncalibrated mean projections:", scen_name[[scen]]),
               freq = FALSE, xlim = xlim_dist, #c(-30,80),
               breaks = seq( from = floor(min(myem[[scen]]$mean[ , paste0("y",yy) ])),
                             to = ceiling(max(myem[[scen]]$mean[ , paste0("y",yy) ])), by = 1),
               col = "cornflowerblue", xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines( density(myem[[scen]]$mean[,paste0("y",yy)]) )


          # Note: myem_nroy for calibration of mean
          hist(myem[[scen]]$mean[ myem_nroy[[scen]], paste0("y",yy) ],
               main = paste("History matched mean projections:", scen_name[[scen]]),
               freq = FALSE, xlim = xlim_dist,
               breaks = seq( from = floor(min(myem[[scen]]$mean[ , paste0("y",yy) ])),
                             to = ceiling(max(myem[[scen]]$mean[ , paste0("y",yy) ])), by = 1),
               col = "cornflowerblue", xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines( density(myem[[scen]]$mean[ myem_nroy[[scen]], paste0("y",yy)]) )

        }
      } # scenario loop
    } #years
  } # all plots

  if (plot_level >= 2) {

    for (yy in yy_plot ) {

      for (scen in scenario_list) {

        if (data_type == "posterior") {

          # Full projections
          hist(projections[[scen]][ ,paste0("y",yy) ],
               main = paste( "Uncalibrated final projections:", scen_name[[scen]] ),
               freq = FALSE, xlim = xlim_dist, #c(-30,80),
               breaks = seq( from = floor(min(projections[[scen]][ , paste0("y",yy) ])),
                             to = ceiling(max(projections[[scen]][ , paste0("y",yy) ])),
                             by = 1),
               col = "cornflowerblue", xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines(density(projections[[scen]][,paste0("y",yy)]))

          # Note: proj_nroy for final projections
          hist(projections[[scen]][ proj_nroy[[scen]], paste0("y",yy) ],
               main = paste( "History matched final projections:", scen_name[[scen]] ),
               freq = FALSE, xlim = xlim_dist, # c(-30,80),
               seq( from = floor(min(projections[[scen]][ , paste0("y",yy) ])),
                    to = ceiling(max(projections[[scen]][ , paste0("y",yy) ])), by = 1),
               col = "cornflowerblue", xlab = paste("Sea level contribution at",yy,"(cm SLE)"))
          lines(density(projections[[scen]][ proj_nroy[[scen]], paste0("y",yy)]))

        } # posterior
      } # end scenario loop

    } # years

  } # plot level

} # END FUNCTION
