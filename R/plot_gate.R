plot_gate <- function(batch_list,
                      path_gs,
                      path_project,
                      marker,
                      path_dir_save_base) {
  # get all length-two combinations of marker
  # (i.e. all pairs of markers)
  gs <- flowWorkspace::load_gs(path_gs)
  comb_marker <- combn(marker, 2, simplify = FALSE)
  if (dir.exists(path_dir_save_base)) {
    unlink(path_dir_save_base, recursive = TRUE)
  }
  marker_lab <- UtilsCytoRSV::chnl_lab(
    flowWorkspace::gh_pop_get_data(gs[[1]])
  )

  for (i in seq_along(comb_marker)) {
    marker_pair <- comb_marker[[i]]
    path_dir_save_marker <- file.path(
      path_dir_save_base, paste0(marker_pair, collapse = "_")
    )
    dir.create(path_dir_save_marker, recursive = TRUE)
    ind_lab <- stats::setNames(
      paste0("ind_", unlist(batch_list)),
      unlist(batch_list)
    )
    for (batch in batch_list) {
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