# Plot correlation between actual (simulated) and StimGate-estimated proportions.
# Creates scatter plots comparing the known true proportions of cytokine-positive
# cells with the proportions estimated by StimGate, showing correlation metrics.
#
# Arguments:
#   path_dir_save_base - Base directory where plots should be saved
#   stats_tbl_bs - Tibble containing both actual and StimGate-estimated proportions
#
# Returns:
#   The path_dir_save_base directory path (after saving all plots)
plot_corr <- function(path_dir_save_base,
                      stats_tbl_bs) {
  set_zero_vec <- c("set_zero", "no_set_zero")
  # Clean up and create output directory
  if (dir.exists(path_dir_save_base)) {
    unlink(path_dir_save_base, recursive = TRUE)
  }
  dir.create(path_dir_save_base, recursive = TRUE)
  # Create plots with and without setting negative values to zero
  for (set_zero in set_zero_vec) {
    path_p <- file.path(path_dir_save_base, paste0("p-", set_zero, ".png"))
    # Pivot data to long format for plotting
    plot_tbl <- stats_tbl_bs |>
      tidyr::pivot_longer(
        cols = c("prop_bs_actual", "prop_bs_stimgate"),
        names_to = "grp",
        values_to = "prop_bs"
      )
    # Optionally set negative proportions to zero
    if (set_zero == "set_zero") {
      plot_tbl <- plot_tbl |>
        dplyr::mutate(
          prop_bs = pmax(
            prop_bs, 0
          )
        )
    }
    # Create overall correlation plot
    p <- UtilsGGSV::ggcorr(
      data = plot_tbl |>
        dplyr::mutate(
          id = paste0(sample_ind, "-", cyt)
        ),
      grp = c("grp"),
      y = "prop_bs",
      grp_base = "prop_bs_actual",
      id = "id",
      corr_method = c("concordance", "pearson", "spearman"),
      limits_equal = TRUE,
      limits_expand = list(0),
      abline = TRUE,
      label_id = TRUE
    ) +
      geom_vline(
        xintercept = 0, linetype = "dashed", color = "gray25"
      ) +
      geom_hline(
        yintercept = 0, linetype = "dashed", color = "gray25"
      )
    # Save overall plot
    ggplot2::ggsave(
      filename = path_p,
      plot = p,
      width = 15,
      height = 15,
      units = "cm",
      dpi = 300
    )
    # Create separate plots for each cytokine combination
    cmbn_vec <- unique(plot_tbl$cyt)
    for (cmbn in cmbn_vec) {
      path_p <- file.path(
        path_dir_save_base,
        paste0("p-", set_zero, "-", cmbn, ".png")
      )
      # Filter to this cytokine combination
      plot_tbl_cmbn <- plot_tbl |>
        dplyr::filter(cyt == cmbn)
      # Create correlation plot for this combination
      p <- UtilsGGSV::ggcorr(
        data = plot_tbl_cmbn,
        grp = c("grp"),
        y = "prop_bs",
        grp_base = "prop_bs_actual",
        id = "sample_ind",
        corr_method = c("concordance", "pearson", "spearman"),
        limits_equal = TRUE,
        limits_expand = list(0),
        abline = TRUE,
        label_id = TRUE
        ) +
        geom_vline(
          xintercept = 0, linetype = "dashed", color = "gray25"
        ) +
        geom_hline(
          yintercept = 0, linetype = "dashed", color = "gray25"
        )
      # Save plot for this combination
      ggplot2::ggsave(
        filename = path_p,
        plot = p,
        width = 15,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  }
  path_dir_save_base
}