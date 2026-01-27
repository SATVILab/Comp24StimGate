# _targets.R file
library(targets)
for (x in list.files(here::here("R"), pattern = "R$|r$", full.names = TRUE)) {
  source(x)
}
targets::tar_option_set(
  # attach packages in targets
  packages = c("ggplot2", "tibble", "projr"),
  imports = c("stimgate")
)

# DESCRIPTION:
# This is the sim_loop targets pipeline - an extension of sim_test that runs multiple
# simulation iterations across different scenarios to compare StimGate against baseline
# tail-gate methods. The pipeline:
# 1. Runs 5 iterations each of "easy" and "poor_separation" scenarios
#    - easy: well-separated positive/negative populations (expr_mean_pos = 2)
#    - poor_separation: overlapping populations (expr_mean_pos = 0.5, higher variance)
# 2. For each iteration:
#    a. Generates simulated flow data using get_gatingset_from_scratch()
#    b. Computes ground truth statistics from known positive cell indices
#    c. Runs StimGate automated gating
#    d. Runs tail-gate baseline with multiple tolerance thresholds (10^-9 to 10^-1)
#    e. Combines all statistics into a single table per iteration
# 3. Aggregates results across iterations to compute correlation metrics
#    - Concordance, Pearson, and Spearman correlations
#    - Compares estimated vs actual proportions for each gating method
# 4. Produces overall correlation summaries for downstream plotting
#
# The pipeline is computationally intensive due to the nested loops over simulations
# and tail-gate tolerance values. Outputs are cached in _tmp/targets for reuse.
#
# This pipeline extends sim_test by adding:
# - Multiple simulation scenarios (easy vs poor_separation)
# - Tail-gate baseline comparisons
# - Statistical aggregation across iterations

list(
  # specify _projr directories
  # ------------------
  # Get project directories from projr configuration
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
  # Run 5 simulation iterations for the "easy" scenario
  # Each iteration generates fresh simulated data, runs StimGate and tail-gate,
  # and combines results into a single statistics table
  tar_target(
    stats_tbl_bs_easy,
    {
      set.seed(1234)
      purrr::map_df(
        seq_len(5),
        function(i) {
          message("Running simulation ", i)
          # Generate simulated GatingSet with "easy" scenario parameters
          # Creates flowSet with well-separated positive/negative populations
          dir_cache_sim <- file.path(dir_cache, i)
          sim_data <- get_gatingset_from_scratch(
            dir_cache = dir_cache_sim,
            scenario = "easy"
          )

          # Compute ground truth statistics from known positive cell indices
          stats_tbl_bs_actual <- get_stats_bs_actual(
            batch_list = sim_data$batch_list,
            chnl_list = sim_data$chnl_list
          )

          # Run StimGate automated gating and extract statistics
          path_project <- file.path(dir_cache_sim, "stimgate")
          Sys.setenv("STIMGATE_INTERMEDIATE" = "TRUE")
          invisible(stimgate::stimgate_gate(
            .data = flowWorkspace::load_gs(sim_data$path_gs),
            path_project = path_project,
            pop_gate = "root",
            batch_list = sim_data$batch_list,
            chnl = names(sim_data$chnl_list),
            debug = TRUE
          ))

          # Extract StimGate statistics
          stats_tbl_bs_stimgate <- get_stats_tbl_bs_stimgate(
            path_project,
            chnl = names(sim_data$chnl_list)
          )

          # Run tail-gate baseline with multiple tolerance thresholds
          # Tests 9 different thresholds from 10^-9 to 10^-1
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

              # Convert to bootstrap format with tolerance-specific column name
              stats_tbl_bs_tg <- get_stats_tbl_bs_tg(stats_tbl_tg)
              cn_new <- paste0(
                "prop_bs_tg_", abs(log10(x))
              )
              stats_tbl_bs_tg[[cn_new]] <- stats_tbl_bs_tg[["prop_bs_tg"]]
              stats_tbl_bs_tg[["prop_bs_tg"]] <- NULL
              stats_tbl_bs_tg
            }
          )
          # Filter out failed gating attempts
          stats_list_bs_tg <- stats_list_bs_tg[
            vapply(stats_list_bs_tg, Negate(is.null), logical(1))
          ]
          # Join all tail-gate results across tolerance thresholds
          stats_tbl_bs_tg <- stats_list_bs_tg[[1]]
          for (j in seq_along(stats_list_bs_tg)[-1]) {
            stats_tbl_bs_tg <- stats_tbl_bs_tg |>
              dplyr::full_join(
                stats_list_bs_tg[[j]],
                by = c("sample_ind", "cyt", "type")
              )
          }

          # Combine actual, StimGate, and tail-gate statistics into one table
          stats_tbl <- stats_tbl_bs_actual |>
            dplyr::full_join(
              stats_tbl_bs_stimgate,
              by = c("sample_ind", "cyt", "type")
            ) |>
            dplyr::full_join(
              stats_tbl_bs_tg,
              by = c("sample_ind", "cyt", "type")
            )
          # Add simulation iteration number
          stats_tbl |>
            dplyr::mutate(
              sim = i
            ) |>
            dplyr::select(sim, dplyr::everything())
        }
      )
    }
  ),
  # Run 5 simulation iterations for the "poor_separation" scenario
  # Same structure as easy scenario, but with overlapping positive/negative populations
  tar_target(
    stats_tbl_bs_poor_separation,
    {
      set.seed(1234)
      purrr::map_df(
        seq_len(5),
        function(i) {
          message("Running simulation ", i)
          # Generate simulated GatingSet with "poor_separation" scenario parameters
          # Creates flowSet with overlapping positive/negative populations (harder to gate)
          dir_cache_sim <- file.path(dir_cache, i)
          sim_data <- get_gatingset_from_scratch(
            dir_cache = dir_cache_sim,
            scenario = "poor_separation"
          )

          # Compute ground truth statistics from known positive cell indices
          stats_tbl_bs_actual <- get_stats_bs_actual(
            batch_list = sim_data$batch_list,
            chnl_list = sim_data$chnl_list
          )

          # Run StimGate automated gating and extract statistics
          path_project <- file.path(dir_cache_sim, "stimgate")
          Sys.setenv("STIMGATE_INTERMEDIATE" = "TRUE")
          invisible(stimgate::stimgate_gate(
            .data = flowWorkspace::load_gs(sim_data$path_gs),
            path_project = path_project,
            pop_gate = "root",
            batch_list = sim_data$batch_list,
            chnl = names(sim_data$chnl_list),
            debug = TRUE
          ))
          # Extract StimGate statistics
          stats_tbl_bs_stimgate <- get_stats_tbl_bs_stimgate(
            path_project,
            chnl = names(sim_data$chnl_list)
          )

          # Run tail-gate baseline with multiple tolerance thresholds
          # Tests 9 different thresholds from 10^-9 to 10^-1
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
              } # else {
              # message("Successfully gated with tol = ", x)
              # }

              stats_tbl_tg <- get_stats_tbl_tg(
                gate_tbl = tg_obj$gate_tbl,
                chnl_list = sim_data$chnl_list,
                batch_list = sim_data$batch_list,
                path_gs = tg_obj$path_gs
              )

              # Convert to bootstrap format with tolerance-specific column name
              stats_tbl_bs_tg <- get_stats_tbl_bs_tg(stats_tbl_tg)
              cn_new <- paste0(
                "prop_bs_tg_", abs(log10(x))
              )
              stats_tbl_bs_tg[[cn_new]] <- stats_tbl_bs_tg[["prop_bs_tg"]]
              stats_tbl_bs_tg[["prop_bs_tg"]] <- NULL
              stats_tbl_bs_tg
            }
          )
          # Filter out failed gating attempts
          stats_list_bs_tg <- stats_list_bs_tg[
            vapply(stats_list_bs_tg, Negate(is.null), logical(1))
          ]
          # Join all tail-gate results across tolerance thresholds
          stats_tbl_bs_tg <- stats_list_bs_tg[[1]]
          for (j in seq_along(stats_list_bs_tg)[-1]) {
            stats_tbl_bs_tg <- stats_tbl_bs_tg |>
              dplyr::full_join(
                stats_list_bs_tg[[j]],
                by = c("sample_ind", "cyt", "type")
              )
          }

          # Combine actual, StimGate, and tail-gate statistics into one table
          stats_tbl <- stats_tbl_bs_actual |>
            dplyr::full_join(
              stats_tbl_bs_stimgate,
              by = c("sample_ind", "cyt", "type")
            ) |>
            dplyr::full_join(
              stats_tbl_bs_tg,
              by = c("sample_ind", "cyt", "type")
            )
          # Add simulation iteration number
          stats_tbl |>
            dplyr::mutate(
              sim = i
            ) |>
            dplyr::select(sim, dplyr::everything())
        }
      )
    }
  ),
  # Compute per-simulation correlation metrics for the easy scenario
  # Calculates concordance, Pearson, and Spearman correlations between
  # actual and estimated proportions for StimGate and each tail-gate threshold
  tar_target(
    corr_tbl_by_sim_easy, get_corr_by_sim(stats_tbl_bs_easy)
  ),
  # Compute per-simulation correlation metrics for the poor_separation scenario
  tar_target(
    corr_tbl_by_sim_poor_separation,
    get_corr_by_sim(stats_tbl_bs_poor_separation)
  ),
  # Aggregate correlation metrics across all simulations for easy scenario
  # Computes mean, SD, and 95% CI for each gating method and correlation type
  tar_target(
    corr_tbl_overall_easy, get_corr_tbl_overall(corr_tbl_by_sim_easy)
  ),
  # Aggregate correlation metrics across all simulations for poor_separation scenario
  tar_target(
    corr_tbl_overall_poor_separation,
    get_corr_tbl_overall(corr_tbl_by_sim_poor_separation)
  )
)
