# _targets.R file
library(targets)
# Source all helper functions from R/ directory
for (x in list.files(here::here("R"), pattern = "R$|r$", full.names = TRUE)) {
  source(x)
}
targets::tar_option_set(
  # attach packages in targets
  packages = c("ggplot2", "tibble", "projr"),
  imports = "stimgate"
)

# DESCRIPTION:
# This is the sim_test targets pipeline - a minimal workflow demonstrating StimGate
# gating on simulated flow cytometry data. The pipeline:
# 1. Loads a base flowSet from HDCytoData
# 2. Generates simulated cytokine expression for BC1 and BC2 markers
# 3. Creates a GatingSet for StimGate analysis
# 4. Plots raw expression distributions
# 5. Runs StimGate automated gating
# 6. Computes bootstrap statistics comparing actual vs StimGate-estimated proportions
# 7. Generates correlation plots and gating visualizations
#
# This pipeline should always complete successfully and serves as the first validation
# after environment setup.

list(
  # specify _projr directories
  # ------------------
  # Get project directories from projr configuration
  tar_target(
    dir_raw_data, projr::projr_path_get_dir("raw-data"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_cache, projr::projr_path_get_dir("cache", "sim_density"),
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
  # Load base flowSet from HDCytoData (Bodenmiller BCR-XL dataset)
  tar_target(fs, get_fs()),

  # Generate simulated cytokine data for BC1 and BC2 channels
  # Returns a list where each element contains fs,
  # ind_list, resp_tbl, batch_list
  tar_target(chnl_list, get_chnl_list(fs = fs)),

  # Extract batch groupings (which samples belong to each batch)
  tar_target(batch_list, chnl_list[[1]]$batch_list),

  # Extract the final flowSet with all simulated channels
  tar_target(fs_gate, chnl_list[[length(chnl_list)]]$fs),

  # Create and save GatingSet from the simulated flowSet
  tar_target(path_gs, get_gatingset(
    fs = chnl_list[[length(chnl_list)]]$fs,
    dir_cache = dir_cache
  ),
  format = "file"
  ),

  # Plot raw expression densities for each marker (stimulated vs unstimulated)
  tar_target(
    path_plot_raw,
    plot_raw(
      batch_list = batch_list,
      chnl_list = chnl_list,
      fs = chnl_list[[length(chnl_list)]]$fs,
      path_dir_save = file.path(dir_cache, "plot", "raw")
    ),
    format = "file"
  ),


  # Plot inter-point distances
  tar_target(
    path_plot_interpoint_few,
    .plot_adjacent_dist_plot(
      c(2, 5, 10, 20, 50, 100, 200, 1e3, 5e3),
      path_dir_save = file.path(dir_cache, "plot", "interpoint")
    )
  ),

  # Plot std normal density
  tar_target(
    path_plot_std_normal_density,
    .plot_std_normal_density(
      break_x = c(0, 0.5, 1, 2, 3),
      path_dir_save = file.path(dir_cache, "plot", "std_normal")
    )
  ),

  tar_target(
    path_plot_bw, plot_bw(
      path_dir_save = file.path(dir_cache, "plot", "bw")
    )
  )
)
