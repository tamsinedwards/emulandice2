#' plot_bayesian: plot prior and posterior distributions
#'
#' @description Plot prior and posterior density estimates from Bayesian
#' calibration.
#'
#' @export

plot_bayesian <- function() {

  # Loop over plot years
  for (yy in yy_plot) {

    ymax <- 0

    for (scen in scenario_list) {

      # FUTURE: mean projections
      if (plot_level > 1) {
        prior_dens_future <- density( myem[[scen]]$mean[, paste0("y",yy) ] )
        post_dens_future <- density( myem[[scen]]$mean[, paste0("y",yy)], weights = myem_weights[[scen]] )
        plot( post_dens_future, main = paste("Mean", yy, scen_name[[scen]]),
              xlim = sle_lim[[yy]], col = AR6_rgb[[scen]])
        lines( prior_dens_future )
      }

      # FUTURE: final projections
      if (FALSE) {
        proj_future <- data.frame(p = projections[[scen]][, paste0("y",yy)],
                                  w = proj_weights[[scen]], bins = 50)
        # Prior
        ggplot(proj_future, aes(p, after_stat(density))) + geom_histogram(binwidth = 2) +
          theme_bw() + geom_density() + ggtitle("2100 uncalibrated")
        # Posteriors
        ggplot(proj_future, aes(p, weight = w, after_stat(density))) +
          geom_histogram(binwidth = 2, color = "red", bg = "red") + theme_bw() +
          geom_density(color = "red") + ggtitle("2100 calibrated")
      }

      prior_dens_future <- density( projections[[scen]][, paste0("y",yy)] )
      post_dens_future <- density( projections[[scen]][, paste0("y",yy)], weights = proj_weights[[scen]] )

      plot( post_dens_future, main = paste("Final projections at", yy, "for", scen_name[[scen]]),
            xlim = sle_lim[[yy]], col = AR6_rgb[[scen]], lwd = 1.5,
            xlab = paste("Sea level contribution at",yy,"(cm SLE)") )
      lines( prior_dens_future, col = "darkgrey" )
      text( sle_lim[[yy]][1] + 0.68*(sle_lim[[yy]][2] - sle_lim[[yy]][1]),
            0.93*max(post_dens_future$y), pos = 4, "Prior", col = "darkgrey")
      text( sle_lim[[yy]][1] + 0.68*(sle_lim[[yy]][2] - sle_lim[[yy]][1]), 0.85*max(post_dens_future$y), pos = 4, "Posterior", col = AR6_rgb[[scen]])

      ymax <- max( c(ymax, max(post_dens_future$y) ) )



    } # scenarios

  } # years


}
