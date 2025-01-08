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

    # Mean projections ---------------------------------------------------------
    for (scen in scenario_list) {

      # FUTURE: prior and posterior mean projections
      if (plot_level > 1) {

        prior_dens_future <- density( myem[[scen]]$mean[, paste0("y",yy) ] )
        post_dens_future <- density( myem_post[[scen]]$mean[, paste0("y",yy)] )

        plot( post_dens_future, main = paste("Mean projections at", yy, "for", scen_name[[scen]]),
              xlim = sle_lim[[yy]], col = AR6_rgb[[scen]])
        lines( prior_dens_future )

        # Legend
        text( sle_lim[[yy]][1] + 0.68*(sle_lim[[yy]][2] - sle_lim[[yy]][1]),
              0.93*max(post_dens_future$y), pos = 4, "Prior", col = "darkgrey")
        text( sle_lim[[yy]][1] + 0.68*(sle_lim[[yy]][2] - sle_lim[[yy]][1]),
              0.85*max(post_dens_future$y), pos = 4, "Posterior", col = AR6_rgb[[scen]])

      }

      # Final projections ------------------------------------------------------

      # FUTURE: final prior and posterior projections
      prior_dens_future <- density( projections[[scen]][, paste0("y",yy)] )
      post_dens_future <- density( proj_post[[scen]][, paste0("y",yy)] )

      # Posterior
      plot( post_dens_future, main = paste("Final projections at", yy, "for", scen_name[[scen]]),
            xlim = sle_lim[[yy]], col = AR6_rgb[[scen]], lwd = 1.5,
            xlab = paste("Sea level contribution at",yy,"(cm SLE)") )

      # Prior
      lines( prior_dens_future, col = "darkgrey" )

      # Legend
      text( sle_lim[[yy]][1] + 0.68*(sle_lim[[yy]][2] - sle_lim[[yy]][1]),
            0.93*max(post_dens_future$y), pos = 4, "Prior", col = "darkgrey")
      text( sle_lim[[yy]][1] + 0.68*(sle_lim[[yy]][2] - sle_lim[[yy]][1]),
            0.85*max(post_dens_future$y), pos = 4, "Posterior", col = AR6_rgb[[scen]])

      ymax <- max( c(ymax, max(post_dens_future$y) ) )

    } # scenarios

  } # years


}
