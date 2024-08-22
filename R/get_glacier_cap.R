#' get_glacier_cap:
#'
#' @description
#' Load max contribution for glacier region estimated by Farinotti et al.
#'
#' @param region RGI region
#'
#' @returns `get_glacier_cap()` returns maximum contribution (cm SLE)
#'
#' @export

get_glacier_cap <- function(region) {

  # Mean values (mm SLE)
  max_glacier <- NA
  if (reg == "RGI01") max_glacier <- 43.3
  if (reg == "RGI02") max_glacier <- 2.6
  if (reg == "RGI03") max_glacier <- 64.8
  if (reg == "RGI04") max_glacier <- 20.5
  if (reg == "RGI05") max_glacier <- 33.6
  if (reg == "RGI06") max_glacier <- 9.1
  if (reg == "RGI07") max_glacier <- 17.3
  if (reg == "RGI08") max_glacier <- 0.7
  if (reg == "RGI09") max_glacier <- 32.0
  if (reg == "RGI10") max_glacier <- 0.3
  if (reg == "RGI11") max_glacier <- 0.3
  if (reg == "RGI12") max_glacier <- 0.2
  if (reg == "RGI13") max_glacier <- 7.9
  if (reg == "RGI14") max_glacier <- 6.9
  if (reg == "RGI15") max_glacier <- 2.1
  if (reg == "RGI16") max_glacier <- 0.2
  if (reg == "RGI17") max_glacier <- 12.8
  if (reg == "RGI18") max_glacier <- 0.2
  if (reg == "RGI19") max_glacier <- 69.4

  cat("_____________________________________\n", file = logfile_build, append = TRUE)
  cat(sprintf("get_glacier_cap: glacier region cannot contribute more than %.1f mm SLE\n", max_glacier),
      file = logfile_build, append = TRUE)

  # Return in cm
  return( max_glacier / 10.0 )

}
