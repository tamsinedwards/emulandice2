#' weight_glacier_cap:
#'
#' @description
#' Weight posterior projections with max contribution for glacier region estimated by Farinotti et al.
#'
#' @returns `weight_glacier_cap()` returns weighted projections
#'
#' @export

weight_glacier_cap <- function(proj_glac) {

  # Old method: atom at threshold
  proj_glac[ proj_glac > glacier_cap ] <- glacier_cap

  # Draft code - not used yet xxx
  if (FALSE) {
    glacier_cap_sd <- 0.1*glacier_cap
    n_sd <- 1

    # Differences between projections and glacier cap, normalised by glacier cap uncertainty
    ee <- (proj_glac - glacier_cap) / glacier_cap_sd
    #print(proj_glac)
    #print( ee )

    # Time-dependent weight, capped at zero so only pulls down from above cap, not up to cap
    ww <- max( exp( ee ) / (1 + exp( ee )), 0)
    #print( ww )

    # If above threshold, pull down (3 sigma hard limit)
    if (ww < 0) {
      proj_glac <- ww * ( glacier_cap + n_sd*glacier_cap_sd) + (1 - ee)
    }
  }

  return(proj_glac)

}
