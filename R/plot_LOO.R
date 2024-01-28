#' plot_LOO: plot LOO figures.
#'
#' @description Plot leave-one-out outputs.
#'
#' @export

plot_loo <- function() {

  par(mfrow = c(1,2))

  # For each LOO timeslice
  for ( yind in names(loo_mean) ) {

    yy <- strsplit(yind,split = "y")[[1]][2]
    #print( paste("LOO validation plots:", yy))

    # Get index for subset
    N_k_index <- !is.na(loo_mean[[yind]])

    # Wrong index for red (just tidier)
    ww <- wrong[[ yind ]]

    # Standardised errors
    # could save as list from do_loo bit of main instead!
    #loo_std_errs <- (loo_valid[[yind]]$mean - ice_data[ , yind]) / loo_valid[[yind]]$sd
    loo_std_errs <- (loo_mean[[yind]] - ice_data[ , yind]) / loo_sd[[yind]]

    # Expand plot range for uncertainty interval
    # This works because lower bound always negative
    ylim_loo <- c(sle_lim[[yy]][1], sle_lim[[yy]][2] * 1.1)

    plot( ice_data[ , yind], loo_mean[[yind]],
          xlim = ylim_loo, ylim = ylim_loo,
          xaxs = "i", yaxs = "i",
          pch = 20, xlab = paste("Simulated sea level contribution at",yy,"(cm SLE)"),
          ylab = paste("Emulated sea level contribution at",yy,"(cm SLE)"),
          main = paste("Leave one out validation:", i_s, yy))
    abline ( a = 0, b = 1 )
    if (i_s == "GLA") {
      abline( h = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
      abline( v = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
    }

    # +/- 2 s.d. error bars
    arrows( ice_data[ , yind], loo_mean[[yind]] - 2*loo_sd[[yind]],
            ice_data[ , yind], loo_mean[[yind]] + 2*loo_sd[[yind]],
            code = 3, angle = 90, lwd = 0.4, length = 0.02 )

    # Replot over in red for those that missed
    points( ice_data[ ww, yind], loo_mean[[yind]][ww], #loo_valid[[yind]]$mean[ww],
            pch = 20, col = "red")
    arrows( ice_data[ ww, yind],
            loo_mean[[yind]][ww] - 2*loo_sd[[yind]][ww], # loo_valid[[yind]]$mean[ww] - 2*loo_valid[[yind]]$sd[ww],
            ice_data[ ww, yind],
            loo_mean[[yind]][ww] + 2*loo_sd[[yind]][ww], # loo_valid[[yind]]$mean[ww] + 2*loo_valid[[yind]]$sd[ww],
            code = 3, angle = 90, lwd = 0.6, length = 0.02, col = "red" )

    # Range of standardised errors (can get very big!)
    max_range <- range(loo_std_errs, na.rm=TRUE)
    max_range[1] <- floor(max_range[1])
    max_range[2] <- ceiling(max_range[2])
    hist( loo_std_errs, xlim = c(-6, 6), breaks = seq(from = max_range[1], to = max_range[2], by = 0.2),
          main = "Standardised LOO errors", col = "darkgrey")

    # Now plot by value of input
    # xxx need to suppress plotting all sims when N_k_subset
    for (pp in colnames(ice_design)) {
      # Sort order
      param_sort <- sort(ice_design[,pp], index.return = TRUE)$ix

      plot( ice_design[ param_sort, pp], loo_mean[[yind]][param_sort], # loo_valid[[yind]]$mean[param_sort],
            xlim = range(ice_design[ , pp]), ylim = ylim_loo,
            pch = 20, xlab = pp,
            ylab = paste("Emulated sea level contribution at",yy,"(cm SLE)"),
            main = paste("LOO ordered by input:",yy))
      abline( h = 0 )
      if (i_s == "GLA") {
        abline( h = max_glaciers[[reg]], col = "darkred", lwd = 0.5, lty = 5)
      }

      # +/- 2 s.d. error bars
      arrows( ice_design[ param_sort, pp],
              loo_mean[[yind]][param_sort] - 2*loo_sd[[yind]][param_sort], #loo_valid[[yind]]$mean[param_sort] - 2*loo_valid[[yind]]$sd[param_sort],
              ice_design[ param_sort, pp],
              loo_mean[[yind]][param_sort] + 2*loo_sd[[yind]][param_sort], # loo_valid[[yind]]$mean[param_sort] + 2*loo_valid[[yind]]$sd[param_sort],
              code = 3, angle = 90, lwd = 0.6, length = 0.02 )

      # Overplot simulations in blue
      points( ice_design[ param_sort, pp], ice_data[ param_sort, yind],
              pch = 20, col = "blue")

      plot( ice_design[ param_sort, pp], loo_std_errs[ param_sort ],
            xlim = range(ice_design[ , pp]), ylim = c(-6,6),
            pch = 20, xlab = pp, cex = 1.2,
            ylab = paste("Emulated - simulated sea level contributio at",yy,"(cm SLE)"),
            main = paste("LOO std errors by input value:",yy))
      abline ( h = 0 )

    }

  } # year loop


}


