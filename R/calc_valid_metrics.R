#' calc_valid_metrics: summarise LOO/TVT predictions and write CSV
#' (a refactoring by Cursor)
#'
#' @description
#' Compute aggregate and by-scenario validation metrics for one year.
#' Appends wide rows to `*_validation.csv`. Used for both LOO and TVT.
#'
#' @param simulated Numeric vector of simulator values (one year).
#' @param emu_mean Numeric vector of emulator means (same length).
#' @param emu_sd Numeric vector of emulator sds (same length).
#' @param scenario Scenario labels (same length).
#' @param row_mask Logical mask of rows to include (LOO `N_k_index`); default all.
#' @param region Character region label for CSV.
#' @param year Year label (numeric or character).
#' @param csv_file Path to validation CSV (header written if file is new).
#' @param logfile Optional build logfile; pointer written when CSV is created.
#'
#' @return Invisibly, a data.frame of rows written (`all` + each scenario).
#'   `n == 0` groups are skipped. `sd_std_err` is `NA` when `n < 2`.
#' @export

calc_valid_metrics <- function(simulated, emu_mean, emu_sd, scenario,
                               row_mask = NULL,
                               region, year,
                               csv_file, logfile = NULL) {

  # Some checks and setup
  stopifnot(length(simulated) == length(emu_mean),
            length(simulated) == length(emu_sd),
            length(simulated) == length(scenario))
  if (is.null(row_mask)) row_mask <- rep(TRUE, length(simulated))
  stopifnot(length(row_mask) == length(simulated), is.logical(row_mask))
  scenario <- as.character(scenario)

  # Function to calculate summary statistics: coverage; MAE; mean, s.d. and max of standardised errors,
  summarise_one <- function(sim, mu, sdv) {
    n <- length(sim)
    stopifnot(n > 0L)
    err <- mu - sim
    std <- err / sdv
    miss <- sim > (mu + 2 * sdv) | sim < (mu - 2 * sdv)
    data.frame(
      region = region,
      year = year,
      n = n,
      coverage_pct = (1 - mean(miss)) * 100.0, # miss is T/F so 1/0
      mean_abs_err = mean(abs(err)),
      mean_std_err = mean(std),
      sd_std_err = ifelse(n > 1, sd(std), NA),
      min_abs_std_err = min(abs(std)),
      max_abs_std_err = max(abs(std)),
      corr_tau = if (n > 1L) unname(cor.test(sim, mu, method = "kendall")$estimate) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  # Rows to keep
  keep <- row_mask & is.finite(simulated) & is.finite(emu_mean) & is.finite(emu_sd)
  sim_k <- simulated[keep]
  mu_k <- emu_mean[keep]
  sd_k <- emu_sd[keep]
  scen_k <- scenario[keep]

  # Calculate statistics across all test simulations (LOO: all; TVT: test_set)
  agg <- summarise_one(sim_k, mu_k, sd_k)
  agg$scenario <- "all"

  # Store
  rows <- list()
  rows[[1]] <- agg

  # Add metrics by scenario
  for (sc in sort(unique(scen_k))) {
    ii <- scen_k == sc
    one <- summarise_one(sim_k[ii], mu_k[ii], sd_k[ii])
    one$scenario <- sc
    rows[[length(rows) + 1L]] <- one
  }

  # Write to validation CSV
  out <- do.call(rbind, rows)
  out <- out[, c("region", "year", "scenario", "n",
                 "coverage_pct", "mean_abs_err", "mean_std_err",
                 "sd_std_err", "min_abs_std_err", "max_abs_std_err", "corr_tau")]

  new_file <- !file.exists(csv_file)
  if (!is.null(logfile) && new_file) {
    cat(sprintf("\nWriting validation metrics CSV: %s\n", csv_file),
        file = logfile, append = TRUE)
  }

  utils::write.table(out, file = csv_file, sep = ",",
                     row.names = FALSE, col.names = new_file,
                     append = !new_file, quote = TRUE)

  # Return metrics (without printing)
  invisible(out)

}
