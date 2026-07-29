# Plotting functions -----------------------------------------------------------
# Internal so no need to export

# Mapping of scenarios when plotting e.g. simulations together
# Suggested by Cursor
scen_match <- function(scen) {
  aliases <- list(
    SSP126        = c("RCP26", "SSP126"),
    SSP245        = c("RCP45", "SSP245"),
    `SSP534-over` = c("SSP534-over", "SSP534-over-recon"),
    SSP585        = c("RCP85", "SSP585")
  )
  if (scen %in% names(aliases)) aliases[[scen]] else scen
}

# AddScale not currently used
AddScale <- function(legbreaks, leglabels, col, xlim, ylim, cex = par("cex")) {

  n <- length(legbreaks)
  stopifnot(length(col) == n - 1)
  dy <- diff(ylim) / (n - 1)
  ybot <- seq(from = ylim[1], by = dy, length.out = n - 1)

  # Palette levels and box
  rect(rep(xlim[1], n - 1), ybot, rep(xlim[2], n - 1), ybot + dy,
       col = col, border = "grey",
       xpd = NA, lwd = 0, ljoin = 1
  )
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col = NA, border = par("fg"),
       xpd = NA, lwd = 0.4, ljoin = 1)

  labcex <- cex

  # labels (auto)
  if (is.null(leglabels)) {
    db <- diff(range(legbreaks)) / (n - 1)
    pp <- pretty(legbreaks)
    pp <- pp[pp >= legbreaks[1] + db / 2 & pp <= legbreaks[n] - db / 2]
    xx <- rep(xlim[2] + 0.002, length(pp))
    yy <- ylim[1] + diff(ylim) * (pp - legbreaks[1]) / (diff(range(legbreaks)))
  } else {
    # (prescribed)
    pp <- leglabels
    xx <- rep(xlim[2] + 0.002, length(pp))
    yy <- seq(from = ylim[1], by = dy, length.out = n)
    if (length(leglabels) == length(legbreaks) - 2) labcex <- 0.8 * cex
  }

  # text and ticks
  text(xx, yy, pp, cex = labcex, xpd = NA, pos = 4)
  sapply(yy - 0.00001, function(ytick) {
    lines(c(xlim[2], xlim[2] + 0.007), c(ytick, ytick), lwd = 0.4)
  })
}
