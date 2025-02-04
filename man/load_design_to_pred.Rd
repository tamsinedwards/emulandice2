% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_design_to_pred.R
\name{load_design_to_pred}
\alias{load_design_to_pred}
\title{load_design_to_pred: construct design for emulator projections}
\usage{
load_design_to_pred(design_name, N_samp = NA)
}
\arguments{
\item{design_name}{Requested design: SSPs, uniform temperatures over original
ensemble ranges, or main effects.}

\item{N_samp}{# Number of bins per axis for continuous parameters

# Default is used for main effects; passed value is used for fixed_temp
For AR6_2LM sample size is set by number of FaIR samples}
}
\value{
`load_design_to_pred()` returns requested designs for each list
member (SSP, or input for main effects).
}
\description{
Construct requested design for emulator projections by sampling inputs.
}
