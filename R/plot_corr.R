plot_corr <- function(path_dir_save_base,
                      stats_tbl_bs) {
  set_zero_vec <- c("set_zero", "no_set_zero")
  if (dir.exists(path_dir_save_base)) {
    unlink(path_dir_save_base, recursive = TRUE)
  }
  dir.create(path_dir_save_base, recursive = TRUE)
  for (set_zero in set_zero_vec) {
    path_p <- file.path(path_dir_save_base, paste0("p-", set_zero, ".png"))
    plot_tbl <- stats_tbl_bs |>
      tidyr::pivot_longer(
        cols = c("prop_bs_actual", "prop_bs_gate"),
        names_to = "grp",
        values_to = "prop_bs"
      )
    if (set_zero == "set_zero") {
      plot_tbl <- plot_tbl |>
        dplyr::mutate(
          prop_bs = pmax(
            prop_bs, 0
          )
        )
    }
    p <- UtilsGGSV::ggcorr(
      data = plot_tbl |>
        dplyr::mutate(
          id = paste0(sample_ind, "-", cyt_combn)
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

    ggplot2::ggsave(
      filename = path_p,
      plot = p,
      width = 15,
      height = 15,
      units = "cm",
      dpi = 300
    )
    cmbn_vec <- unique(plot_tbl$cyt_combn)
    for (cmbn in cmbn_vec) {
      path_p <- file.path(
        path_dir_save_base,
        paste0("p-", set_zero, "-", cmbn, ".png")
      )
      plot_tbl_cmbn <- plot_tbl |>
        dplyr::filter(cyt_combn == cmbn)
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