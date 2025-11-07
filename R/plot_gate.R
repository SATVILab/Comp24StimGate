# Plot StimGate gating results showing gates overlaid on bivariate scatter plots.
# Creates scatter plots for each pair of markers, showing StimGate's automated gates
# and how they partition the cells into positive and negative populations.
#
# Arguments:
#   batch_list - List defining which sample indices belong to each batch
#   path_gs - File path to the saved GatingSet
#   path_project - Directory where StimGate saved its results
#   marker - Vector of marker/channel names to plot
#   path_dir_save_base - Base directory where plots should be saved
#
# Returns:
#   The path_dir_save_base directory path (after saving all plots)
plot_gate <- function(batch_list,
                      path_gs,
                      path_project,
                      marker,
                      path_dir_save_base) {
  # Get all pairwise combinations of markers
  gs <- flowWorkspace::load_gs(path_gs)
  comb_marker <- combn(marker, 2, simplify = FALSE)
  # Clean up output directory
  if (dir.exists(path_dir_save_base)) {
    unlink(path_dir_save_base, recursive = TRUE)
  }
  # Get marker labels for plot axes
  marker_lab <- UtilsCytoRSV::chnl_lab(
    flowWorkspace::gh_pop_get_data(gs[[1]])
  )
  # Create plots for each marker pair
  for (i in seq_along(comb_marker)) {
    marker_pair <- comb_marker[[i]]
    # Create subdirectory for this marker pair
    path_dir_save_marker <- file.path(
      path_dir_save_base, paste0(marker_pair, collapse = "_")
    )
    dir.create(path_dir_save_marker, recursive = TRUE)
    # Create sample index labels
    ind_lab <- stats::setNames(
      paste0("ind_", unlist(batch_list)),
      unlist(batch_list)
    )
    # Create a plot grid for each batch
    for (batch in batch_list) {
      # Use StimGate's plotting function to show gates
      p_grid <- stimgate::stimgate_plot(
        ind = batch,
        ind_lab = ind_lab,
        .data = gs,
        path_project = path_project,
        marker = marker_pair,
        limits_expand = list(c(0, 4)),
        limits_equal = TRUE,
        marker_lab = marker_lab,
      )
      # Save plot for this batch
      fn <- paste0(paste0(batch, collapse = "_"), ".png")
      ggplot2::ggsave(
        file.path(path_dir_save_marker, fn),
        p_grid,
        units = "cm",
        width = 20,
        height = 20
      )
    }
  }
  path_dir_save_base
}