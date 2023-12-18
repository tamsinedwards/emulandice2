#' emulator_predict: Emulator predictions for design
#'
#' @description
#' Predict for all points in design.
#'
#' @param design_prior Design for projections, i.e. sampled priors on all inputs.
#'
#' @returns `emulator_predict()` returns emulator mean and sd values for design.
#'
#' @export

emulator_predict <- function(design_prior) {

  myem <- emu_mv( as.matrix(design_prior), type = "var")

  # Add year column names to emulator predictions
  colnames(myem$mean) <- paste0("y", years_em)
  colnames(myem$sd) <- paste0("y", years_em)

  return(myem)

}
