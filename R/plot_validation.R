#' plot_validation: plot validation figures.
#'
#' @description Plot validation outputs.
#'
#' @export

plot_valid <- function(valid_type) {

  # LOO or TVT
  stopifnot(valid_type %in% c("LOO", "TVT"))
  par(mfrow = c(1,2))

  # xxx Could instead use same names in emulator_build.R for both?
  if (valid_type == "LOO") {
    valid_sims <- ice_data
    valid_design <- ice_design
    valid_emu <- loo_mean
    valid_emu_sd <- loo_sd
    valid_title <- paste0("LOO validation (N_k = ", N_k, "):")
  }
  if (valid_type == "TVT") {
    valid_sims <- ice_data[test_set, ]
    valid_design <- ice_design[test_set, , drop = FALSE]
    valid_emu <- test_mean
    valid_emu_sd <- test_sd
    valid_title <- paste0("Test set validation (N = ", length(test_set), ")")
  }

  # Check we are comparing same number of things
  stopifnot(nrow(valid_design) == length(valid_emu[[1]]))

  dot_grey <- grey(0.2,0.4)

  # For each requested year
  for ( yind in names(valid_emu) ) {

    yy <- strsplit(yind,split = "y")[[1]][2]

    # Get index for subset
    N_k_index <- !is.na(valid_emu[[yind]])

    # Wrong index
    ww <- wrong[[ yind ]]

    # Also uses frac_right

    # Standardised errors
    # could save as list from do_loo to save recalculating
    # Keep as full ensemble length here so can plot against parameter values
    valid_std_err <- (valid_emu[[yind]] - valid_sims[ , yind]) / valid_emu_sd[[yind]]

    # Expand plot range for uncertainty interval
    # This works because lower bound always negative
    ylim_valid <- c(sle_lim[[yy]][1], sle_lim[[yy]][2] * 1.1)

    # EMULATED VS SIMULATED
    plot( valid_sims[ , yind], valid_emu[[yind]],
          xlim = ylim_valid, ylim = ylim_valid,
          xaxs = "i", yaxs = "i", col = dot_grey,
          pch = 20, xlab = paste("Simulated sea level contribution at",yy,"(cm SLE)"),
          ylab = paste("Emulated sea level contribution at",yy,"(cm SLE)"),
          main = valid_title)
    abline ( a = 0, b = 1 )
    if (i_s == "GLA") {
      abline( h = glacier_cap, col = "lightgrey", lwd = 0.25, lty = 5)
      abline( v = glacier_cap, col = "lightgrey", lwd = 0.25, lty = 5)
    }

    # +/- 2 s.d. error bars
    arrows( valid_sims[ , yind], valid_emu[[yind]] - 2*valid_emu_sd[[yind]],
            valid_sims[ , yind], valid_emu[[yind]] + 2*valid_emu_sd[[yind]],
            code = 3, angle = 90, lwd = 0.4, length = 0.02, col = dot_grey )

    # Replot over in bright colour for those that missed
    points( valid_sims[ ww, yind], valid_emu[[yind]][ww],
            pch = 20, col = "coral2")
    arrows( valid_sims[ ww, yind],
            valid_emu[[yind]][ww] - 2*valid_emu_sd[[yind]][ww],
            valid_sims[ ww, yind],
            valid_emu[[yind]][ww] + 2*valid_emu_sd[[yind]][ww],
            code = 3, angle = 90, lwd = 0.4, length = 0.02, col = "coral2" )

    # Coverage
    col_text <- ifelse(frac_right < 0.9, "coral2", "black")
    text( ylim_valid[1], ylim_valid[1] + 0.95*diff(range(ylim_valid)), pos = 4,
          sprintf("Coverage: %.0f%%", frac_right*100.0), col = col_text)

    # Correlation: move calculation into emulator_build.R to print? xxx
    tau <- cor.test(valid_sims[ ,yind ], valid_emu[[yind]],
                    alternative = "t", methods = "kendall")
    col_text <- ifelse(tau$estimate < 0.8, "darkred", "black")
    text( ylim_valid[1], ylim_valid[1] + 0.88*diff(range(ylim_valid)), pos = 4,
          sprintf("Kendall's tau: %.2f", tau$estimate), col = col_text)


    # HISTOGRAM OF RESIDUALS
    # Range of standardised errors (can get very big!)
    max_range <- range(valid_std_err, na.rm=TRUE)
    max_range[1] <- floor(max_range[1])
    max_range[2] <- ceiling(max_range[2])

    hist( valid_std_err, xlim = max_range, xlab = "Standardised errors",
          breaks = seq(from = max_range[1], to = max_range[2], by = 0.2),
          main = "Standardised validation errors", col = "deepskyblue4")
    hist( valid_std_err[valid_std_err > 3 | valid_std_err < -3],
          breaks = seq(from = max_range[1], to = max_range[2], by = 0.2),
          col = "coral2", add = TRUE)

    # Now plot by value of input
    # xxx need to suppress plotting all sims when N_k_subset i.e. not LOO
    for (pp in colnames(valid_design)) {

      # Sort order
      param_sort <- sort(valid_design[,pp], index.return = TRUE)$ix

      # EMULATED AND SIMULATED VS INPUT
      plot( valid_design[ param_sort, pp], valid_emu[[yind]][param_sort],
            xlim = range(valid_design[ , pp]), ylim = ylim_valid,
            pch = 20, xlab = pp, col = dot_grey,
            ylab = paste("Emulated sea level contribution at",yy,"(cm SLE)"),
            main = paste("Validation by input:",yy))
      abline( h = 0 )
      if (i_s == "GLA") {
        abline( h = glacier_cap, col = "darkred", lwd = 0.5, lty = 5)
      }

      # +/- 2 s.d. error bars
      arrows( valid_design[ param_sort, pp],
              valid_emu[[yind]][param_sort] - 2*valid_emu_sd[[yind]][param_sort],
              valid_design[ param_sort, pp],
              valid_emu[[yind]][param_sort] + 2*valid_emu_sd[[yind]][param_sort],
              code = 3, angle = 90, lwd = 0.6, length = 0.02, col = dot_grey )

      # Cursor: colour vector by scenario
      col_scen <- vapply( valid_sims$scenario[param_sort] , function(s) {
        col <- AR6_rgb[[s]]
        if (is.null(col)) "grey50" else col
      }, character(1))

      # Overplot simulations - coloured by scenario
      points( valid_design[ param_sort, pp], valid_sims[ param_sort, yind],
              pch = 21, col = col_scen, bg = NULL)

      # RESIDUALS - coloured by scenario
      plot( valid_design[ param_sort, pp], valid_std_err[ param_sort ],
            xlim = range(valid_design[ , pp]), ylim = max_range,
            pch = 20, xlab = pp, cex = 1.2, col = col_scen,
            ylab = paste("Standardised error at",yy),
            main = paste("Standardised errors by input:",yy))
      abline ( h = 0 )

      # Shade +/- 3
      rect( min(valid_design[ param_sort, pp]) - 0.1*diff(range(valid_design[ param_sort, pp])), 3,
            max(valid_design[ param_sort, pp]) + 0.1*diff(range(valid_design[ param_sort, pp])), -3,
            col = grey(0.2,0.04), border = "black", lwd = 0.5, lty = 5)

      # Legend
      legy <- max_range[2]
      for (scen in sort(unique(valid_sims[,"scenario"]))) {
        mycol <- ifelse(scen %in% names(AR6_rgb), AR6_rgb[[scen]], "grey50")
        text(min(valid_design[, pp]), legy, pos = 4, scen, col = mycol)
        legy <- legy - 0.05 * diff(max_range)
      }

    }

  } # year loop


}
