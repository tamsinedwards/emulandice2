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

  # Farinotti et al. (2019) Table 1
  # "The potential sea-level equivalent (SLE) accounts for the ice portion presently located below sea level"

  # Total: 324.3 ± 84.1 mm SLE
  max_glacier <- NA

  # Mean values and presumable 1 s.d. (mm SLE)
  if (reg == "RGI01") max_glacier <- c(43.3, 11.2)
  if (reg == "RGI02") max_glacier <- c(2.6, 0.7)
  if (reg == "RGI03") max_glacier <- c(64.8, 16.8)
  if (reg == "RGI04") max_glacier <- c(20.5, 5.3)
  if (reg == "RGI05") max_glacier <- c(33.6, 8.7)
  if (reg == "RGI06") max_glacier <- c(9.1, 2.4)
  if (reg == "RGI07") max_glacier <- c(17.3, 4.5)
  if (reg == "RGI08") max_glacier <- c(0.7, 0.2)
  if (reg == "RGI09") max_glacier <- c(32.0, 8.3)
  if (reg == "RGI10") max_glacier <- c(0.3, 0.1)
  if (reg == "RGI11") max_glacier <- c(0.3, 0.1)
  if (reg == "RGI12") max_glacier <- c(0.2, 0.0)
  if (reg == "RGI13") max_glacier <- c(7.9, 2.0)
  if (reg == "RGI14") max_glacier <- c(6.9, 1.8)
  if (reg == "RGI15") max_glacier <- c(2.1, 0.5)
  if (reg == "RGI16") max_glacier <- c(0.2, 0.1)
  if (reg == "RGI17") max_glacier <- c(12.8, 3.3)
  if (reg == "RGI18") max_glacier <- c(0.2, 0.0)
  if (reg == "RGI19") max_glacier <- c(69.4, 18.0)

  cat("_____________________________________\n", file = logfile_build, append = TRUE)
  cat(sprintf("get_glacier_cap: glacier region estimate is %.1f ± %.1f mm SLE\n\n", max_glacier[1], max_glacier[2]),
      file = logfile_build, append = TRUE)

  # Return mean in cm SLE
  return( max_glacier[1] / 10.0 )

}
