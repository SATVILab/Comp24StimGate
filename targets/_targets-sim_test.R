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
  tar_target(fs, get_fs()),
  tar_target(chnl_list, get_chnl_list(fs = fs)),
  tar_target(batch_list, chnl_list[[1]]$batch_list),
  tar_target(fs_gate, chnl_list[[length(chnl_list)]]$fs),
  tar_target(path_gs, get_gatingset(
    fs = chnl_list[[length(chnl_list)]]$fs,
    dir_cache = dir_cache
  ),
  format = "file"
  ),
  tar_target(
    path_plot_raw,
    plot_raw(
      batch_list = batch_list,
      chnl_list = chnl_list,
      fs = chnl_list[[length(chnl_list)]]$fs,
      path_dir_save = file.path(dir_cache, "plot", "sim_test", "raw")
    ),
    format = "file"
  ),
  tar_target(
    path_project,
    gate(
      path_gs = path_gs,
      chnl_list = chnl_list,
      batch_list = batch_list,
      path_project = file.path(dir_cache, "stimgate", "sim_test")
    ),
    format = "file"
  ),
  tar_target(
    stats_tbl_bs,
    get_stats_bs(
      batch_list = batch_list,
      chnl_list = chnl_list,
      path_project = path_project
    )
  ),
  tar_target(
    path_plot_corr,
    plot_corr(
      path_dir_save_base = file.path(dir_cache, "plot", "sim_test", "corr"),
      stats_tbl_bs = stats_tbl_bs
    ),
    format = "file"
  ),
  tar_target(
    path_plot_gate,
    plot_gate(
      batch_list = batch_list,
      path_gs = path_gs,
      path_project = path_project,
      marker = names(chnl_list),
      path_dir_save_base = file.path(dir_cache, "plot", "sim_test", "stimgate")
    ),
    format = "file"
  )
)
