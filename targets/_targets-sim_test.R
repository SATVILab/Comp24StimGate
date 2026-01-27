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
    dir_cache, projr::projr_path_get_dir("cache", "sim_test"),
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
  # Returns a list where each element contains fs, ind_list, resp_tbl, batch_list
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

  # Run StimGate automated gating and save results
  tar_target(
    path_project,
    gate(
      path_gs = path_gs,
      chnl_list = chnl_list,
      batch_list = batch_list,
      path_project = file.path(dir_cache, "stimgate"),
      .debug = TRUE,
      intermediate = TRUE
    ),
    format = "file"
  ),

  # Extract bootstrap statistics from StimGate results
  # Includes single-marker, combination, and any-positive proportions
  tar_target(
    stats_tbl_bs_stimgate,
    get_stats_tbl_bs_stimgate(
      chnl = names(chnl_list),
      path_project = path_project
    )
  ),

  # Compute actual (ground truth) bootstrap statistics from simulation
  tar_target(
    stats_tbl_bs_actual,
    get_stats_bs_actual(
      batch_list = batch_list,
      chnl_list = chnl_list
    )
  ),

  # Join StimGate and actual statistics for comparison
  tar_target(
    stats_tbl_bs,
    stats_tbl_bs_stimgate |>
      dplyr::inner_join(
        stats_tbl_bs_actual,
        by = c("type", "sample_ind", "cyt")
      )
  ),

  # Plot correlation between StimGate and actual proportions
  tar_target(
    path_plot_corr,
    plot_corr(
      path_dir_save_base = file.path(dir_cache, "plot", "corr"),
      stats_tbl_bs = stats_tbl_bs
    ),
    format = "file"
  ),

  # Plot StimGate gates overlaid on bivariate scatter plots
  tar_target(
    path_plot_gate,
    plot_gate(
      batch_list = batch_list,
      path_gs = path_gs,
      path_project = path_project,
      chnl = names(chnl_list),
      path_dir_save_base = file.path(dir_cache, "plot", "stimgate")
    ),
    format = "file"
  )
)
