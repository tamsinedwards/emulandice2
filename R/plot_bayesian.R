#' plot_bayesian: plot prior and posterior distributions
#'
#' @description Plot prior and posterior density estimates from Bayesian
#' calibration.
#'
#' @export

plot_bayesian <- function() {


  # Loop over plot years - duplicates above xxx
  for (yy in yy_plot) {

    ymax <- 0

    for (scen in scenario_list) {

      if (yy == cal_end) xlim_dist = ylim_obs
      if (yy > cal_end && yy <= 2100 ) xlim_dist = ylim
      if (yy > 2100 ) xlim_dist = ylim_max

      # FUTURE: mean projections
      if (write_mean) {
        prior_dens_future <- density( myem[[scen]]$mean[, paste0("y",yy) ] )
        post_dens_future <- density( myem[[scen]]$mean[, paste0("y",yy)], weights = myem_weights[[scen]] )
        plot( post_dens_future, main = paste("Mean", yy, scen_name[[scen]]),
              xlim = xlim_dist, col = AR6_rgb[[scen]])
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
            xlim = xlim_dist, col = AR6_rgb[[scen]], lwd = 1.5)
      lines( prior_dens_future, col = "darkgrey" )
      text( xlim_dist[1] + 0.68*(xlim_dist[2] - xlim_dist[1]),
            0.93*max(post_dens_future$y), pos = 4, "Prior", col = "darkgrey")
      text( xlim_dist[1] + 0.68*(xlim_dist[2] - xlim_dist[1]), 0.85*max(post_dens_future$y), pos = 4, "Posterior", col = AR6_rgb[[scen]])

      ymax <- max( c(ymax, max(post_dens_future$y) ) )



    } # scenarios


    # COMBINED PLOT OF FINAL POSTERIOR SSPS

    plot( 1:3, 1:3, type = "n", main = paste("Final projections at", yy ),
          xlim = xlim_dist, ylim = c(0, ymax),
          xlab = paste("Sea level contribution at ",yy,"(cm SLE)"),
          ylab = "Density")

    yleg <- ymax

    for (scen in scenario_list) {

      post_dens_future <- density( projections[[scen]][, paste0("y",yy)], weights = proj_weights[[scen]] )

      lines( post_dens_future, col = AR6_rgb[[scen]], lwd = 1.5 )

      yleg <- yleg - 0.1*ymax

      text( xlim_dist[1] + 0.6*(xlim_dist[2] - xlim_dist[1]),
            yleg, pos = 4, scen_name[[scen]], col = AR6_rgb[[scen]])

    }

  } # years


}
