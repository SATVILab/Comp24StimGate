# Run StimGate automated gating on a GatingSet to identify cytokine-positive cells.
# This is the core gating function that calls stimgate::stimgate_gate to automatically
# determine gates for each marker, taking into account batch effects and background.
#
# Arguments:
#   chnl_list - List of channel objects (used to get marker names)
#   batch_list - List defining which sample indices belong to each batch
#   path_gs - File path to the saved GatingSet
#   path_project - Directory where StimGate should save its project files and results
#
# Returns:
#   The path_project directory path (where StimGate saves gates and statistics)
gate <- function(chnl_list,
                 batch_list,
                 path_gs,
                 path_project,
                 .debug = FALSE,
                 intermediate = FALSE) {
  # Load the GatingSet from disk
  gs <- suppressMessages(flowWorkspace::load_gs(path_gs))
  # Run StimGate gating algorithm and save results to path_project
  if (.debug) {
    Sys.setenv("STIMGATE_DEBUG" = "TRUE")
    on.exit(Sys.unsetenv("STIMGATE_DEBUG"), add = TRUE)
  }
  if (intermediate) {
    Sys.setenv("STIMGATE_INTERMEDIATE" = "TRUE")
    on.exit(Sys.unsetenv("STIMGATE_INTERMEDIATE"), add = TRUE)
  }
  stimgate::stimgate_gate(
    .data = gs,
    path_project = path_project,
    pop_gate = "root",          # Gate from the root pilopulation
    batch_list = batch_list,    # Account for batch structure
    chnl = names(chnl_list)   # Markers to gate
  )
}

# Run tail-gate gating as a baseline comparison method.
# This function applies a simple tail-gating approach using openCyto's gate_tail method,
# which gates based on a tolerance threshold. Used to compare against StimGate.
#
# Arguments:
#   path_gs - File path to the saved GatingSet
#   chnl_list - List of channel objects (used to get marker names)
#   batch_list - List defining which sample indices belong to each batch (not used)
#   path_project - Directory where results should be saved (not used)
#   path_gs_save - Path where the gated GatingSet should be saved
#   tol - Tolerance parameter for the tail gate algorithm
#
# Returns:
#   A list containing:
#     - path_gs: path to the saved gated GatingSet
#     - gate_tbl: tibble with gate thresholds for each sample and channel
gate_tg <- function(path_gs,
                    chnl_list,
                    batch_list,
                    path_project,
                    path_gs_save,
                    tol) {
  # Load GatingSet
  gs <- flowWorkspace::load_gs(path_gs)
  # Register the tail-gate plugin
  suppressMessages(openCyto::register_plugins(
    fun = cytoUtils:::.gate_tail, methodName = "gate_tail"
  ))
  # Apply tail-gating to each channel and collect gate thresholds
  gate_tbl_tg <- purrr::map_df(names(chnl_list), function(x) {
    # Add gating method to GatingSet
    openCyto::gs_add_gating_method(
      gs = gs,
      alias = x,
      pop = "+",
      parent = "root",
      dims = x,
      gating_method = "gate_tail",
      gating_args = paste0("tol = ", tol)
    )
    # Extract gate thresholds for each sample
    gates_list <- flowWorkspace::gs_pop_get_gate(
      gs, y = paste0("root/", x)
    )
    purrr::map_df(seq_along(gs), function(i) {
      gate <- (gates_list[[i]] |>
        flowWorkspace::filter_to_list())$range[[1]]
      tibble::tibble(ind = i, chnl = x, gate = gate)
    })
  })
  # Clean up and save gated GatingSet
  if (dir.exists(path_gs_save)) {
    unlink(path_gs_save, recursive = TRUE)
  }
  suppressMessages(flowWorkspace::save_gs(
    gs = gs,
    path = path_gs_save,
    compress = TRUE
  ))
  list(
    path_gs = path_gs_save,
    gate_tbl = gate_tbl_tg
  )
}