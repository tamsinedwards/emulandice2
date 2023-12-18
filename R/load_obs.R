#' load_obs: read observational data.
#'
#' @description
#' Load IMBIE or Hugonnet et al. observations:
#' ICE SHEETS: IMBIE FOR AR6 1992-2020
#' Downloaded from https://ramadda.data.bas.ac.uk/repository/entry/show?entryid=77b64c55-7166-4a06-9def-2e400398e452
#' on 28/3/23 via doi https://doi.org/10.5285/77B64C55-7166-4A06-9DEF-2E400398E452
#' in IMBIE preprint https://essd.copernicus.org/preprints/essd-2022-261/essd-2022-261.pdf
#'
#' GLACIERS: Hugonnet et al. (2021) regional files
#' https://www.sedoo.fr/theia-publication-products/?uuid=c428c5b9-df8f-4f86-9b75-e04c778e29b9
#'
#' @returns `load_obs()` returns annual observations with columns ["Year", "SLE",
#' "SLE_sd"]
#'
#' @export


load_obs <- function() {

  if (i_s == "GLA") {

    # Read regional file
    reg_number <- strsplit(reg, split = "RGI")[[1]][2]
    obs_file <- read.csv(paste0(inputs_ext,"GLA/Hugonnet/time_varying_glacier_areas/dh_", reg_number, "_rgi60_reg_cumul.csv"))
    obs_file <- obs_file[ , c("time", "dm", "err_dm") ]

    # Get every 12th value because these have uncertainties - check this is right XXX
    obs_file <- obs_file[ 1 + (0:20*12), ]

    # Convert dates to round years for now
    # XXX Check this is OK - some dates are 31/12 not 1/1
    obs_file[ ,1] <- 2000:2020

    # Convert Gt/yr to cm mass loss
    obs_file[ , 2:3] <- obs_file[ , 2:3] / (10 * 360)
    # Convert to sea level rise
    obs_file[ , 2] <- -1* obs_file[ , 2]

    names(obs_file) <- c("Year", "SLE", "SLE_sd")

  } else {

    # %in% c("GrIS","GIS"))
  if (i_s == "GIS") obs_file <- read.csv(paste0(inputs_ext,"GIS/IMBIE/imbie_greenland_2021_mm.csv"))
  if (i_s == "AIS") obs_file <- read.csv(paste0(inputs_ext,"/AIS/IMBIE/imbie_antarctica_2021_mm.csv"))

  # Pick columns and tidy names
  obs_file <- obs_file[ , c("Year","Cumulative.mass.balance..mm.", "Cumulative.mass.balance.uncertainty..mm.") ]
  names(obs_file)[2:3] <- c("SLE", "SLE_sd")
  obs_file[,3] <- -1 * obs_file[,3]

  }

  # Convert mm to cm
  obs_file[,2:3] <- obs_file[,2:3] / 10

  return(obs_file)



}
