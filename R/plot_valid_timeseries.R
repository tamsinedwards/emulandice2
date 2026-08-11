#' plot_valid_timeseries: emu vs sim timeseries for validation
#' (refactoring of code by Cursor)
#'
#' @description
#' Plot batches of validation timeseries (emulator mean vs simulator) and
#' errors. Used for TVT and LOO.
#'
#' @param valid_type `"LOO"` or `"TVT"`.
#' @param n_plot Number of simulations per batch (default 10).
#' @param n_batches Number of batches. `NULL` = one random sample of `n_plot`.
#'   `"all"` = enough batches to cover all eligible sims.
#' @export

plot_valid_timeseries <- function(valid_type, n_plot = 10, n_batches = NULL) {

  stopifnot(valid_type %in% c("LOO", "TVT"))

  # Validation nrow matches Ytrain / N_sims layout

  # Output from LOO do_loo
  if (valid_type == "LOO") {
    stopifnot(exists("emu_loo", inherits = TRUE))
    emu_mat <- emu_loo
    sim_mat <- as.matrix(Ytrain)
    eligible <- which(rowSums(is.finite(emu_mat$mean)) > 0L)
    title_pref <- paste0("LOO (N_k = ", ifelse(is.na(N_k), "all", N_k), ")")
  }

  # Output from TVT
  if (valid_type == "TVT") {
    stopifnot(exists("emu_test", inherits = TRUE),
              exists("test_data", inherits = TRUE))
    emu_mat <- emu_test
    sim_mat <- as.matrix(test_data)
    eligible <- seq_len(nrow(emu_mat$mean))
    title_pref <- paste0("TVT test (N = ", length(test_set), ")")
  }

  stopifnot(nrow(emu_mat$mean) == nrow(sim_mat),
            ncol(emu_mat$mean) == length(years_em),
            ncol(sim_mat) == length(years_em))

  colnames(emu_mat$mean) <- paste0("y", years_em)
  colnames(sim_mat) <- paste0("y", years_em)

  # Select 1 or more batches of n_plot runs to plot
  n_elig <- length(eligible)
  if (n_elig == 0L) {
    warning("plot_valid_timeseries: no eligible simulations to plot")
    return(invisible(NULL))
  }
  if (is.null(n_batches)) {
    n_batches <- 1L
  } else if (identical(n_batches, "all")) {
    n_batches <- as.integer(max(1L, ceiling(n_elig / n_plot)))
  } else {
    n_batches <- as.integer(n_batches)
  }

  # Sampling: set seed so that selection only depends on eligible, n_plot, n_batches
  set.seed(2024)
  order_ix <- sample(eligible, size = n_elig)
  need <- n_batches * n_plot
  if (length(order_ix) < need) {
    order_ix <- c(order_ix,
                  sample(eligible, need - length(order_ix), replace = TRUE))
  }

  # Dark and transparent colours
  run_cols <- grDevices::hcl.colors(n_plot, palette = "Dark 3")
  sd_cols <- grDevices::hcl.colors(n_plot, palette = "Dark 3", alpha = 0.1)

  # For each batch
  for (b in seq_len(n_batches)) {

    # Rows
    idx <- order_ix[((b - 1L) * n_plot + 1L):(b * n_plot)]

    # Emulator alongside simulator
    plot(years_em, emu_mat$mean[idx[1], ], type = "n",
         main = paste0(title_pref, ": sims (solid) vs emu mean (dashed); batch ", b),
         xlab = "Year", ylab = "Sea level contribution (cm SLE)",
         ylim = extendrange(c(sim_mat[idx, ],
                              emu_mat$mean[idx, ] + 2*emu_mat$sd[idx, ],
                              emu_mat$mean[idx, ] - 2*emu_mat$sd[idx, ]),
                            f = 0.1) )
    abline(h = 0)
    for (ss in seq_along(idx)) {
      lines(years_em, emu_mat$mean[idx[ss], ], col = run_cols[ss], lty = 5)
      polygon(c(years_em, rev(years_em)),
                c(emu_mat$mean[idx[ss], ] + 2*emu_mat$sd[idx[ss], ],
                  rev(emu_mat$mean[idx[ss], ] - 2*emu_mat$sd[idx[ss], ])),
                col = sd_cols[ss], border = NA)
      lines(years_em, sim_mat[idx[ss], ], col = run_cols[ss])
    }

    # Residuals and +/- 2s.d. uncertainties
    # Shows magnitude of emulator uncertainties, e.g. with time, and coverage
    # i.e. whether emulator residuals fall within ± 2s.d.
    # (solid lines mostly within corresponding shading)
    err <- emu_mat$mean[idx, , drop = FALSE] - sim_mat[idx, , drop = FALSE]
    plot(years_em, err[1, ], type = "n",
         main = paste0(title_pref, ": emu minus sim; batch ", b),
         xlab = "Year", ylab = "Emulated minus simulated (cm SLE)",
         ylim = extendrange(c(err,
                              2 * emu_mat$sd[idx, ], -2 * emu_mat$sd[idx, ]),
                            f = 0.1) )

    abline(h = 0)
    for (ss in seq_along(idx)) {
      lines(years_em, err[ss, ], col = run_cols[ss])
      polygon(c(years_em, rev(years_em)),
              c(2*emu_mat$sd[idx[ss], ],
                rev(-2*emu_mat$sd[idx[ss], ])),
              col = sd_cols[ss], border = NA)
    }
  } # batches

  invisible(NULL)
}
