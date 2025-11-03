# _targets.R file
library(targets)
for (x in list.files(here::here("R"), pattern = "R$|r$", full.names = TRUE)) {
  source(x)
}
targets::tar_option_set(
  # attach packages in targets
  packages = c("ggplot2", "tibble", "projr")
)

list(
  # specify _projr directories
  # ------------------
  tar_target(
    dir_raw_data, projr::projr_path_get_dir("raw-data"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_cache, projr::projr_path_get_dir("cache"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_output, projr::projr_path_get_dir("output"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_docs, projr::projr_path_get_dir("docs"),
    cue = tar_cue(mode = "always")
  ),

  # do something
  # ------------------
  tar_target(
    stats_tbl_bs_easy, {
      set.seed(1234)
      purrr::map_df(
        seq_len(5),
        function(i) {
        message("Running simulation ", i)
        # get data
        dir_cache_sim <- file.path(dir_cache, i)
        sim_data <- get_gatingset_from_scratch(
          dir_cache = dir_cache_sim,
          scenario = "easy"
        )

        # get actual stats
        stats_tbl_bs_actual <- get_stats_bs_actual(
          batch_list = sim_data$batch_list,
          chnl_list = sim_data$chnl_list
        )

        # gate using stimgate
        path_project <- file.path(dir_cache_sim, "stimgate")
        invisible(stimgate::stimgate_gate(
          .data = flowWorkspace::load_gs(sim_data$path_gs),
          path_project = path_project,
          pop_gate = "root",
          batch_list = sim_data$batch_list,
          marker = names(sim_data$chnl_list)
        ))

        # get stats from stimgate

        stats_tbl_bs_stimgate <- get_stats_tbl_bs_stimgate(
          path_project, chnl = names(sim_data$chnl_list)
        )

        # gate using tail gate
        stats_list_bs_tg <- purrr::map(
          10^seq(-9, -1), function(x) {
            tg_obj <- try(gate_tg(
              path_gs = sim_data$path_gs,
              chnl_list = sim_data$chnl_list,
              batch_list = sim_data$batch_list,
              path_project = path_project,
              path_gs_save = file.path(dirname(sim_data$path_gs), "gs_tg"),
              tol = x
            ))
            if (inherits(tg_obj, "try-error")) {
              message("Error in gating with tol = ", x)
              return(NULL)
            }

            stats_tbl_tg <- get_stats_tbl_tg(
              gate_tbl = tg_obj$gate_tbl,
              chnl_list = sim_data$chnl_list,
              batch_list = sim_data$batch_list,
              path_gs = tg_obj$path_gs
            )

            stats_tbl_bs_tg <- get_stats_tbl_bs_tg(stats_tbl_tg)
            cn_new <- paste0(
              "prop_bs_tg_", abs(log10(x))
            )
            stats_tbl_bs_tg[[cn_new]] <- stats_tbl_bs_tg[["prop_bs_tg"]]
            stats_tbl_bs_tg[["prop_bs_tg"]] <- NULL
            stats_tbl_bs_tg
          }
        )
        stats_list_bs_tg <- stats_list_bs_tg[
          vapply(stats_list_bs_tg, Negate(is.null), logical(1))
        ]
        stats_tbl_bs_tg <- stats_list_bs_tg[[1]]
        for (j in seq_along(stats_list_bs_tg)[-1]) {
          stats_tbl_bs_tg <- stats_tbl_bs_tg |>
            dplyr::full_join(
              stats_list_bs_tg[[j]],
              by = c("sample_ind", "cyt", "type")
            )
        }

        # get combined stats tbl
        stats_tbl <- stats_tbl_bs_actual |>
          dplyr::full_join(
            stats_tbl_bs_stimgate,
            by = c("sample_ind", "cyt", "type")
          ) |>
          dplyr::full_join(
            stats_tbl_bs_tg,
            by = c("sample_ind", "cyt", "type")
          )
        stats_tbl |>
          dplyr::mutate(
            sim = i
          ) |>
          dplyr::select(sim, dplyr    ::everything())
    })
  }),
  tar_target(
    stats_tbl_bs_poor_separation, {
      set.seed(1234)
      purrr::map_df(
        seq_len(5),
        function(i) {
        message("Running simulation ", i)
        # get data
        dir_cache_sim <- file.path(dir_cache, i)
        sim_data <- get_gatingset_from_scratch(
          dir_cache = dir_cache_sim,
          scenario = "poor_separation"
        )

        # get actual stats
        stats_tbl_bs_actual <- get_stats_bs_actual(
          batch_list = sim_data$batch_list,
          chnl_list = sim_data$chnl_list
        )

        # gate using stimgate
        path_project <- file.path(dir_cache_sim, "stimgate")
        invisible(stimgate::stimgate_gate(
          .data = flowWorkspace::load_gs(sim_data$path_gs),
          path_project = path_project,
          pop_gate = "root",
          batch_list = sim_data$batch_list,
          marker = names(sim_data$chnl_list)
        ))

        # get stats from stimgate
        stats_tbl_bs_stimgate <- get_stats_tbl_bs_stimgate(
          path_project, chnl = names(sim_data$chnl_list)
        )

        # gate using tail gate
        stats_list_bs_tg <- purrr::map(
          10^seq(-9, -1), function(x) {
            tg_obj <- try(gate_tg(
              path_gs = sim_data$path_gs,
              chnl_list = sim_data$chnl_list,
              batch_list = sim_data$batch_list,
              path_project = path_project,
              path_gs_save = file.path(dirname(sim_data$path_gs), "gs_tg"),
              tol = x
            ))
            if (inherits(tg_obj, "try-error")) {
              message("Error in gating with tol = ", x)
              return(NULL)
            }

            stats_tbl_tg <- get_stats_tbl_tg(
              gate_tbl = tg_obj$gate_tbl,
              chnl_list = sim_data$chnl_list,
              batch_list = sim_data$batch_list,
              path_gs = tg_obj$path_gs
            )

            stats_tbl_bs_tg <- get_stats_tbl_bs_tg(stats_tbl_tg)
            cn_new <- paste0(
              "prop_bs_tg_", abs(log10(x))
            )
            stats_tbl_bs_tg[[cn_new]] <- stats_tbl_bs_tg[["prop_bs_tg"]]
            stats_tbl_bs_tg[["prop_bs_tg"]] <- NULL
            stats_tbl_bs_tg
          }
        )
        stats_list_bs_tg <- stats_list_bs_tg[
          vapply(stats_list_bs_tg, Negate(is.null), logical(1))
        ]
        stats_tbl_bs_tg <- stats_list_bs_tg[[1]]
        for (j in seq_along(stats_list_bs_tg)[-1]) {
          stats_tbl_bs_tg <- stats_tbl_bs_tg |>
            dplyr::full_join(
              stats_list_bs_tg[[j]],
              by = c("sample_ind", "cyt", "type")
            )
        }

        # get combined stats tbl
        stats_tbl <- stats_tbl_bs_actual |>
          dplyr::full_join(
            stats_tbl_bs_stimgate,
            by = c("sample_ind", "cyt", "type")
          ) |>
          dplyr::full_join(
            stats_tbl_bs_tg,
            by = c("sample_ind", "cyt", "type")
          )
        stats_tbl |>
          dplyr::mutate(
            sim = i
          ) |>
          dplyr::select(sim, dplyr    ::everything())
    })
  }),
  tar_target(
    corr_tbl_by_sim_easy, get_corr_by_sim(stats_tbl_bs_easy)
  ),
  tar_target(
    corr_tbl_by_sim_poor_separation,
    get_corr_by_sim(stats_tbl_bs_poor_separation)
  ),
  tar_target(
    corr_tbl_overall_easy, get_corr_tbl_overall(corr_tbl_by_sim_easy)
  ),
  tar_target(
    corr_tbl_overall_poor_separation,
    get_corr_tbl_overall(corr_tbl_by_sim_poor_separation)
  )
)