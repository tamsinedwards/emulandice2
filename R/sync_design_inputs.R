# sync_design_inputs
# Sync the dimensions of various matrices and input lists
# for GSAT, continuous and dummy ice inputs
# But NOT for original factor list / factor levels
#
# Used:
# - After dropping GSAT timeslices (drop_temps), before make_emu
# - After make_emu, which can drop inert inputs
#
# keep_names = character vector of columns to retain (order optional but useful)
# remaining arguments - matrices and lists to sync
#
# Code mostly by Cursor AI, replacing my previous sync in emulator_build.R
# after make_emu so that it can be used in multiple places

sync_design_inputs <- function(keep_names,
                               Xtrain, ice_design,
                               temps, temps_list, temps_list_names,
                               ice_cont_list, ice_dummy_list, ice_all_list,
                               input_cont_list,
                               inputs_centre = NULL, inputs_scale = NULL,
                               ice_design_scaled = NULL ) {

  keep_names <- unique(keep_names)

  # Matrices / frames
  Xtrain            <- Xtrain[, colnames(Xtrain) %in% keep_names, drop = FALSE]
  ice_design        <- ice_design[, colnames(ice_design) %in% keep_names, drop = FALSE]
  if (! is.null(ice_design_scaled) ) ice_design_scaled <- ice_design_scaled[, colnames(ice_design_scaled) %in% keep_names, drop = FALSE]

  # Prefer colnames order from the design you care about
  keep_names <- colnames(Xtrain)

  keep_gsat <- temps_list_names %in% keep_names
  if (length(temps_list) > 1) temps <- temps[, keep_gsat, drop = FALSE]
  temps_list <- temps_list[keep_gsat]
  temps_list_names <- temps_list_names[keep_gsat]

  ice_cont_list   <- intersect(ice_cont_list, keep_names)
  input_cont_list <- c(temps_list_names, ice_cont_list) # Rebuild this one, don't intersect

  if (all(is.na(ice_dummy_list))) {
    ice_all_list <- ice_cont_list
  } else {
    ice_dummy_list <- intersect(ice_dummy_list, keep_names)
    ice_all_list <- c(ice_cont_list, ice_dummy_list) # Rebuild too
  }

  if (! is.null(inputs_centre) ) inputs_centre <- inputs_centre[ intersect(names(inputs_centre), input_cont_list) ]
  if (! is.null(inputs_scale) ) inputs_scale  <- inputs_scale[ intersect(names(inputs_scale), input_cont_list) ]

  stopifnot(
    all(temps_list_names %in% colnames(Xtrain)),
    length(temps_list) == length(temps_list_names),
    ncol(temps) == length(temps_list) || length(temps_list) == 1
  )

  list(Xtrain = Xtrain, ice_design = ice_design,
       temps = temps, temps_list = temps_list, temps_list_names = temps_list_names,
       ice_cont_list = ice_cont_list, ice_dummy_list = ice_dummy_list,
       ice_all_list = ice_all_list, input_cont_list = input_cont_list,
       inputs_centre = inputs_centre, inputs_scale = inputs_scale,
       ice_design_scaled = ice_design_scaled )
}
