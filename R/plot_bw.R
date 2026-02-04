plot_bw <- function(path_dir_save) {

  if (dir.exists(path_dir_save)) {
    unlink(path_dir_save, recursive = TRUE)
  }
  dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)

  set.seed(2106)
  # overall plots
  n_cell_list <- list(
    "few" = c(2, 5, 10, 20, 50, 100, 200),
    "many" = c(1e3, 5e3, 1e4, 5e4, 1e5, 5e5, 1e6, 2e6)
  )
  for (j in seq_along(n_cell_list)) {
    n_cell <- n_cell_list[[j]]
    n_cell_dscrptn <- names(n_cell_list)[[j]]
    method_vec <- c("nrd0", "SJ", "hpi", "hpi_deriv_1", "richards")
    bw_tbl <- tibble::tibble(
      n_cell = rep(NA_real_, length(n_cell)),
      nrd0 = rep(NA_real_, length(n_cell)),
      SJ = rep(NA_real_, length(n_cell)),
      hpi = rep(NA_real_, length(n_cell)),
      hpi_deriv_1 = rep(NA_real_, length(n_cell)),
      richards = rep(NA_real_, length(n_cell))
    )
    for (i in seq_along(n_cell)) {
      n_cell_curr <- n_cell[[i]]
      bw_tbl[i, "n_cell"] <- n_cell_curr
      x_vec_curr <- sample_response(
        n = n_cell_curr,
        prop_mean = 0.01,
        prop_sd = 2e-5,
        expr_mean_neg = 0,
        expr_mean_pos = 3,
        expr_sd_pos = 1
      )$expr_vec
      for (method_curr in method_vec) {
        if (method_curr == "nrd0") {
          dens_obj <- density(x_vec_curr)
          bw <- dens_obj$bw
          bw_tbl[i, "nrd0"] <- bw
        } else if (method_curr == "SJ") {
          dens_obj <- density(x_vec_curr, bw = "SJ")
          bw <- dens_obj$bw
          bw_tbl[i, "SJ"] <- bw
        } else if (method_curr == "hpi") {
          bw <- ks::hpi(x_vec_curr)
          dens_obj <- density(x_vec_curr, bw = bw)
          bw_tbl[i, "hpi"] <- bw
        } else if (method_curr == "hpi_deriv_1") {
          bw <- ks::hpi(x_vec_curr, deriv.order = 1)
          dens_obj <- density(x_vec_curr, bw = bw)
          bw_tbl[i, "hpi_deriv_1"] <- bw
        } else if (method_curr == "richards") {
          w_bins <- 10
          bin_width <- diff(range(x_vec_curr)) / sqrt(n_cell_curr)
          bw <- (w_bins / sqrt(12)) * bin_width
          dens_obj <- density(x_vec_curr, bw = bw)
          bw_tbl[i, "richards"] <- bw
        }
        path_p <- file.path(
          path_dir_save, "ind", paste0("n_cell_", n_cell_curr),
          paste0("_method_", method_curr, ".png")
        )
        if (!dir.exists(dirname(path_p))) {
          dir.create(dirname(path_p), recursive = TRUE, showWarnings = FALSE)
        }
        png(path_p)
        plot(
          dens_obj,
          main = paste0(
            "n_cell: ", n_cell_curr,
            ", method: ", method_curr,
            ", bw: ", round(bw, 4))
        )
        dev.off()
      }
    }
    path_p <- file.path(path_dir_save, n_cell_dscrptn, "bandwidth_vs_ncell.png")
    if (!dir.exists(dirname(path_p))) {
      dir.create(dirname(path_p), recursive = TRUE)
    }
    plot_tbl <- bw_tbl |>
      tidyr::pivot_longer(
        cols = -n_cell,
        names_to = "method",
        values_to = "bandwidth"
      )
    x_vec_break <- n_cell
    p <- ggplot(plot_tbl, aes(x = n_cell, y = bandwidth, color = method)) +
      geom_line() +
      geom_point() +
      cowplot::theme_cowplot() +
      scale_x_log10(breaks = x_vec_break) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      cowplot::background_grid() +
      scale_y_continuous(transform = "log2")
    if (n_cell_dscrptn == "many") {
      p <- p + theme(axis.text.x = element_text(angle = 45))
    }
    ggsave(path_p, p, width = 6, height = 4, units = "in")

    # now, make plots using these bandwidths,
    # as if there were varying numbers of
    # cells in the positive population

    n_cell_pos_vec <- c(2, 4, 10, 20, 50, 100, 200)
    bw_tbl_long <- bw_tbl |>
      tidyr::pivot_longer(
        cols = -n_cell,
        names_to = "method",
        values_to = "bandwidth"
      )
    plot_tbl <- purrr::map_df(n_cell_pos_vec, function(n_cell_pos_curr) {
      x_vec_stim <- rnorm(n_cell_pos_curr, mean = 0, sd = 1)
      x_vec_uns <- rnorm(n_cell_pos_curr / 2, mean = 0, sd = 1)
      purrr::map_df(n_cell, function( n_cell_curr) {
        purrr::map_df(method_vec, function(method_curr) {
          bw <- bw_tbl_long |>
            dplyr::filter(n_cell == n_cell_curr, method == method_curr) |>
            dplyr::pull(bandwidth)
          purrr::map_df(c("stim", "uns"), function(stim_condition) {
            x_vec <- if (stim_condition == "stim") {
              x_vec_stim
            } else {
              x_vec_uns
            }
            dens_obj <- density(x_vec, bw = bw)
            tibble::tibble(
              n_cell = n_cell_curr,
              n_cell_pos = n_cell_pos_curr,
              method = method_curr,
              bandwidth = sprintf("%.3f", bw),
              stim_condition = stim_condition,
              x = dens_obj$x,
              y = dens_obj$y,
              # store the raw points for geom_rug (one list entry per panel)
              points = list(x_vec)
            )
          })
        })
      })
    })
    path_dir_save_pos <- file.path(path_dir_save, n_cell_dscrptn, "pos")
    if (dir.exists(path_dir_save_pos)) {
      unlink(path_dir_save_pos, recursive = TRUE)
    }
    dir.create(path_dir_save_pos, recursive = TRUE, showWarnings = FALSE)
    for (method in method_vec) {
      plot_tbl_method <- plot_tbl |>
        dplyr::filter(method == !!method) |>
        dplyr::mutate(
          n_cell_bw = paste0("#: ", n_cell, "\nbw: ", bandwidth),
          n_cell_pos_pretty = signif(n_cell_pos, 3)
        ) |>
      # order by total number of cells
        dplyr::mutate(
          n_cell_bw_order = factor(
            n_cell_bw,
            levels = unique(n_cell_bw[order(n_cell)])
          )
        ) |>
        dplyr::mutate(
          n_cell_pos_pretty_order = factor(
            n_cell_pos_pretty,
            levels = unique(n_cell_pos_pretty[order(n_cell_pos)])
          )
        )

      # prepare point data for geom_rug (one row per raw point, per panel)
      points_tbl_method <- plot_tbl_method |>
        dplyr::select(n_cell, n_cell_pos, n_cell_bw, n_cell_pos_pretty,
                    n_cell_bw_order, n_cell_pos_pretty_order,
                    bandwidth, stim_condition, points) |>
        dplyr::distinct() |>
        tidyr::unnest(cols = c(points)) |>
        dplyr::rename(x_point = points)

      # standard normal curve over the x-range used in this method's data
      xr <- range(plot_tbl_method$x, na.rm = TRUE)
      std_df <- tibble::tibble(x = seq(xr[1], xr[2], length.out = 256)) |>
        dplyr::mutate(y = stats::dnorm(x, mean = 0, sd = 1))

      path_p <- file.path(
        path_dir_save_pos,
        paste0("bandwidth_method_", method, ".png")
      )
      p <- ggplot(
        plot_tbl_method,
        aes(
          x = x, y = y, color = stim_condition
          )
      ) +
        # add KDEs
        geom_line(alpha = 0.8) +
        # add standard normal (dashed blue) in each facet
        ggplot2::geom_line(
          data = std_df,
          ggplot2::aes(x = x, y = y),
          inherit.aes = FALSE,
          color = "#0072B2",
          linetype = "dashed",
          linewidth = 0.6
        ) +
        # add rug to show raw points (bottom)
        ggplot2::geom_rug(
          data = points_tbl_method,
          ggplot2::aes(x = x_point, color = stim_condition),
          inherit.aes = FALSE,
          sides = "b",
          alpha = 0.5
        ) +
        facet_grid(
          n_cell_pos_pretty_order ~ n_cell_bw_order,#,
          labeller = labeller(
          #   n_cell = function(x) {
          #     paste0("#: ", x, "\nbw: ", bandwidth)
          #   },
            n_cell_pos = function(x) {
              paste0("# pos: ", x)
            }
          )
        ) +
        cowplot::theme_cowplot(rel_large = 1, font_size = 12) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        ) +
        cowplot::background_grid() +
        theme(
          strip.background = element_rect(fill = "#FAFAFA", color = "#D3D3D3"),
          legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
        ) +
        scale_colour_manual(
          name = "Condition",
          values = c("stim" = "#CC79A7", "uns" = "#009E73"),
          labels = c("Stimulated", "Unstimulated")
        ) +
        labs(
          x = "Expression",
          y = "Density",
          title = paste0("Bandwidth Method: ", method)
        ) +
        guides(
          color = guide_legend(override.aes = list(linewidth = 2))
        )
      ggsave(path_p, p, width = 18, height = 18, units = "cm")
    }
  }
}

#' Simulate adjacent-point distance quantiles from standard normal
#'
#' @param n_obs_vec integer vector Number(s) of observations per draw. Each value must be >= 2.
#' @param n_draws integer Number of simulation draws. Default is 10000.
#' @param probs numeric vector Probabilities for quantiles. Default is c(0.25, 0.5, 0.75, 0.9, 0.95).
#' @param rng_seed integer or NULL Optional seed for reproducibility. Default is NULL.
#' @return tibble with columns: n_obs, prob, value (quantile of adjacent distances).
#' @examples
#' .sim_adjacent_dist_quantiles(3, n_draws = 1000, rng_seed = 42)
#' @noRd
.sim_adjacent_dist_quantiles <- function(n_obs_vec,
                                         n_draws = 10000L,
                                         probs = c(0.25, 0.5, 0.75, 0.9, 0.95),
                                         rng_seed = NULL) {
  if (any(n_obs_vec < 2)) {
    stop("n_obs_vec values must be >= 2")
  }
  n_draws <- as.integer(n_draws)
  if (length(n_draws) != 1 || n_draws < 1L) {
    stop("n_draws must be a single integer >= 1")
  }
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop("probs must be between 0 and 1")
  }
  if (!is.null(rng_seed)) {
    set.seed(rng_seed)
  }

  res_list <- lapply(n_obs_vec, function(n_obs) {
    # draw matrix: each row is one draw of n_obs standard normals
    mat <- matrix(stats::rnorm(n_draws * n_obs), nrow = n_draws, ncol = n_obs)
    # sort each row
    mat_sorted <- t(apply(mat, 1, sort))
    # compute adjacent differences: columns 2..n_obs minus 1..(n_obs-1)
    diffs <- mat_sorted[, 2:n_obs, drop = FALSE] - mat_sorted[, 1:(n_obs - 1), drop = FALSE]
    diffs_vec <- as.numeric(diffs) # pooled distances across draws
    q_vals <- stats::quantile(diffs_vec, probs = probs, names = FALSE, type = 7, na.rm = TRUE)
    tibble::tibble(
      n_obs = as.integer(n_obs),
      prob = as.numeric(probs),
      value = as.numeric(q_vals)
    )
  })

  dplyr::bind_rows(res_list)
}

#' Plot adjacent-point distance quantiles vs number of observations
#'
#' @param n_obs_vec integer vector Number(s) of observations per draw. Each value must be >= 2.
#' @param n_draws integer Number of simulation draws. Default is 10000.
#' @param probs numeric vector Probabilities for quantiles. Default is c(0.25, 0.5, 0.75, 0.9, 0.95).
#' @param rng_seed integer or NULL Optional seed for reproducibility. Default is NULL.
#' @param path_dir_save character or NULL Directory to save the plot. If NULL, the plot is not saved. Default NULL.
#' @param file_name character Name of the saved plot file (ignored if path_dir_save is NULL). Default "adjacent_dist_vs_nobs.png".
#' @param width,height,units Plot size parameters passed to ggsave when saving.
#' @return ggplot object (invisible when saved).
#' @noRd
.plot_adjacent_dist_plot <- function(n_obs_vec,
                                     n_draws = 10000L,
                                     probs = c(0.25, 0.5, 0.75, 0.9, 0.95),
                                     rng_seed = NULL,
                                     path_dir_save = NULL,
                                     file_name = "adjacent_dist_vs_nobs.png",
                                     width = 6, height = 4, units = "in") {
  if (any(n_obs_vec < 2)) {
    stop("n_obs_vec values must be >= 2")
  }
  n_draws <- as.integer(n_draws)
  if (length(n_draws) != 1 || n_draws < 1L) {
    stop("n_draws must be a single integer >= 1")
  }
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop("probs must be between 0 and 1")
  }

  res <- .sim_adjacent_dist_quantiles(n_obs_vec = n_obs_vec,
                                      n_draws = n_draws,
                                      probs = probs,
                                      rng_seed = rng_seed)

  # make probabilities human-readable and preserve ordering
  res <- res |>
    dplyr::mutate(
      prob_label = factor(
        paste0(round(prob * 100, 0), "%"),
        levels = paste0(round(probs * 100, 0), "%")
      )
    )

  p <- ggplot2::ggplot(
    res,
    ggplot2::aes(x = n_obs, y = value, color = prob_label, group = prob_label)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    cowplot::theme_cowplot() +
    cowplot::background_grid() +
    ggplot2::scale_x_log10(breaks = n_obs_vec) +
    ggplot2::labs(
      x = "Number of observations (n)",
      y = "Adjacent-point distance",
      color = "Quantile",
      title = "Adjacent-point distance quantiles vs n"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    ) +
    scale_y_continuous(
      trans = "log2",
      breaks = c(0.001, 0.01, 0.05, 0.2, 0.5, 1, 2, 5),
      limits = c(0.0001, 6)
    )

  if (!is.null(path_dir_save)) {
    if (!dir.exists(path_dir_save)) {
      dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)
    }
    path_p <- file.path(path_dir_save, file_name)
    ggplot2::ggsave(path_p, p, width = width, height = height, units = units)
    invisible(path_p)
  } else {
    p
  }
}

#' Plot standard normal density with specified y-axis breaks and labels on right-hand side
#'
#' @param break_x numeric vector x-values at which to compute density breakpoints. Default c(0.05, 0.2, 0.5, 1, 2, 2.5).
#' @param decimals integer Number of decimals to round the density breakpoints. Default is 3.
#' @param n integer Number of points to evaluate the density curve. Default is 1024.
#' @param path_dir_save character or NULL Directory to save the plot. If NULL the plot is returned (default NULL).
#' @param file_name character Name of file when saving. Default is "std_normal_density.png".
#' @param width,height,units Plot size parameters passed to ggsave when saving.
#' @return ggplot object (returned) or path to saved file (invisibly returned when saved).
#' @noRd
.plot_std_normal_density <- function(
  break_x = c(0.05, 0.2, 0.5, 1, 2, 2.5),
  decimals = 3L,
  n = 1024L,
  path_dir_save = NULL,
  file_name = "std_normal_density.png",
  width = 6, height = 4, units = "in"
) {
  break_x <- as.numeric(break_x)
  decimals <- as.integer(decimals)
  n <- as.integer(n)
  if (length(break_x) < 1) {
    stop("break_x must be a numeric vector of length >= 1")
  }
  if (decimals < 0L) stop("decimals must be non-negative integer")
  if (n < 2L) stop("n must be >= 2")

  # compute curve for right-hand side only (x >= 0)
  x <- seq(0, 4, length.out = n)
  y <- stats::dnorm(x)
  df <- tibble::tibble(x = x, y = y)

  # compute rounded breakpoints (for axis) and true positions (for labels)
  y_breaks <- sort(round(stats::dnorm(break_x), decimals))

  label_df <- tibble::tibble(
    x = as.numeric(break_x),
    y = stats::dnorm(break_x)
  ) |>
    dplyr::mutate(
      x_lbl = formatC(x, format = "f", digits = decimals),
      y_lbl = formatC(round(y, decimals), format = "f", digits = decimals),
      label = paste0("x=", x_lbl, "\n", "y=", y_lbl)
    )

  # build plot using consistent styling and add label points + ggrepel labels
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    cowplot::background_grid() +
    ggplot2::scale_x_continuous(
      breaks = c(0, break_x),
      labels = c("0", as.character(round(break_x, decimals)))
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = scales::label_number(accuracy = 10^-decimals),
      limits = c(max(0, min(y_breaks) * 0.9), max(y, na.rm = TRUE) * 1.05)
    ) +
    ggplot2::geom_point(data = label_df, ggplot2::aes(x = x, y = y), color = "#D55E00", size = 2) +
    ggrepel::geom_label_repel(
      data = label_df,
      ggplot2::aes(x = x, y = y, label = label),
      nudge_x = 0.25,
      box.padding = 0.35,
      point.padding = 0.3,
      segment.color = "grey50",
      min.segment.length = 0
    ) +
    ggplot2::labs(
      x = "x",
      y = "Density",
      title = "Standard Normal Density (right-hand side)"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "none"
    )

  if (!is.null(path_dir_save)) {
    if (!dir.exists(path_dir_save)) dir.create(path_dir_save, recursive = TRUE, showWarnings = FALSE)
    path_p <- file.path(path_dir_save, file_name)
    ggplot2::ggsave(path_p, p, width = width, height = height, units = units)
    invisible(path_p)
  } else {
    p
  }
}

