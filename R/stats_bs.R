# Extract actual (ground truth) bootstrap statistics from simulated data.
# Computes background-subtracted proportions for single markers, combinations,
# and any-positive populations based on the known positive cell indices from simulation.
#
# Arguments:
#   batch_list - List defining which sample indices belong to each batch
#   chnl_list - List of channel objects containing simulated data and indices
#
# Returns:
#   A tibble with columns:
#     - type: "single", "combn", or "any" 
#     - cyt: cytokine/marker combination string (e.g., "BC1(La139)Dd~+~BC2(Pr141)Dd~-~")
#     - sample_ind: sample index
#     - prop_bs_actual: actual background-subtracted proportion
get_stats_bs_actual <- function(batch_list,
                                chnl_list) {
  # Get combination-level statistics (all marker combinations)
  stats_tbl_combn <- get_stats_bs_actual_combn(
    batch_list = batch_list,
    chnl_list = chnl_list
  )
  # Derive single-marker statistics by marginalizing over other markers
  stats_tbl_single <- get_stats_bs_single(
    stats_bs_combn = stats_tbl_combn,
    chnl = names(chnl_list),
    grp = c("type", "sample_ind"),
    resp = "prop_bs_actual"
  )
  # Derive any-positive statistics (positive for any marker)
  stats_tbl_any <- get_stats_bs_any(
    stats_bs_combn = stats_tbl_combn,
    chnl = names(chnl_list),
    grp = c("type", "sample_ind"),
    resp = "prop_bs_actual"
  )
  # Combine all three types
  stats_tbl_any |>
    dplyr::bind_rows(
      stats_tbl_single
    ) |>
    dplyr::bind_rows(stats_tbl_combn) |>
    dplyr::mutate(
      sample_ind = as.character(sample_ind)
    )
}

# Extract actual combination-level statistics from simulated data.
# Gets background-subtracted proportions for all marker combinations from the
# simulated ground truth data (excluding unstimulated samples).
#
# Arguments:
#   batch_list - List defining which sample indices belong to each batch
#   chnl_list - List of channel objects containing simulated data
#
# Returns:
#   A tibble with combination statistics (type = "combn") for stimulated samples
get_stats_bs_actual_combn <- function(batch_list,
                                      chnl_list) {
  # Get indices of stimulated samples (exclude last sample in each batch)
  pos_vec_sample <- lapply(batch_list, function(x) x[-length(x)]) |>
    unlist()
  # Calculate all combinations and filter to stimulated samples
  calc_resp_combn(chnl_list = chnl_list) |>
    dplyr::rename(cyt_combn = chnl) |>
    dplyr::filter(sample_ind %in% pos_vec_sample) |>
    dplyr::select(
      sample_ind, cyt_combn, prop_bs
    ) |>
    dplyr::mutate(sample_ind = as.character(sample_ind)) |>
    dplyr::rename(prop_bs_actual = prop_bs) |>
    dplyr::mutate(type = "combn") |>
    dplyr::rename(cyt = cyt_combn)
}

# Extract StimGate bootstrap statistics from the gating results.
# Retrieves proportions estimated by StimGate and derives single-marker and 
# any-positive statistics from the combination data.
#
# Arguments:
#   path_project - Directory where StimGate saved its results
#   chnl - Vector of channel/marker names
#
# Returns:
#   A tibble with columns:
#     - type: "single", "combn", or "any"
#     - cyt: cytokine/marker combination string
#     - sample_ind: sample index
#     - prop_bs_stimgate: StimGate-estimated background-subtracted proportion
get_stats_tbl_bs_stimgate <- function(path_project,
                                      chnl) {
  # Get combination-level statistics from StimGate output
  stats_tbl_combn <- get_stats_bs_stimgate_combn(path_project)
  # Derive single-marker statistics
  stats_tbl_single <- get_stats_bs_single(
    stats_bs_combn = stats_tbl_combn,
    chnl = chnl,
    grp = c("type", "sample_ind"),
    resp = "prop_bs_stimgate"
  )
  # Derive any-positive statistics
  stats_tbl_any <- get_stats_bs_any(
    stats_bs_combn = stats_tbl_combn,
    chnl = chnl,
    grp = c("type", "sample_ind"),
    resp = "prop_bs_stimgate"
  )
  # Combine all three types
  stats_tbl_any |>
    dplyr::bind_rows(
      stats_tbl_single
    ) |>
    dplyr::bind_rows(stats_tbl_combn) |>
    dplyr::mutate(
      sample_ind = as.character(sample_ind)
    )
}

# Extract combination-level statistics from StimGate output.
# Reads StimGate results and filters to combination patterns (e.g., BC1+BC2+).
#
# Arguments:
#   path_project - Directory where StimGate saved its results
#
# Returns:
#   A tibble with combination statistics (type = "combn")
get_stats_bs_stimgate_combn <- function(path_project) {
  # Read StimGate statistics file
  stimgate::get_stats(path_project) |>
    dplyr::rename(sample_ind = ind, prop_bs_stimgate = prop_bs) |>
    dplyr::select(
      sample_ind, cyt_combn, prop_bs_stimgate
    ) |>
    # Filter to combinations (contain "~+~" separator)
    dplyr::filter(grepl("~\\+~", cyt_combn)) |>
    dplyr::mutate(
      type = "combn"
    ) |>
    dplyr::rename(
      cyt = cyt_combn
    ) |>
    dplyr::select(type, cyt, sample_ind, prop_bs_stimgate)
}

# Derive single-marker statistics from combination data by marginalization.
# Sums over all combinations involving the focal marker being positive,
# effectively marginalizing out other markers.
#
# Arguments:
#   stats_bs_combn - Tibble with combination-level statistics
#   chnl - Vector of channel/marker names
#   grp - Grouping variables (e.g., c("type", "sample_ind"))
#   levels - Positive/negative pattern markers (default: c("~+~", "~-~"))
#   resp - Response variable name (e.g., "prop_bs_actual" or "prop_bs_stimgate")
#
# Returns:
#   A tibble with single-marker statistics (type = "single")
get_stats_bs_single <- function(stats_bs_combn,
                                chnl,
                                grp = NULL,
                                levels = c("~+~", "~-~"),
                                resp) {
  # For each channel, sum over combinations where that channel is positive
  purrr::map_df(chnl, function(x) {
    # Markers to marginalize over (all except focal marker)
    chnl_to_sum <- setdiff(chnl, x)
    stats_bs_combn |>
      UtilsCytoRSV::sum_over_markers(
        grp = grp,
        cmbn = "cyt",
        levels = levels,
        markers_to_sum = chnl_to_sum,
        resp = resp
      )
  }) |>
    # Keep only combinations where focal marker is positive
    dplyr::filter(grepl(levels[[1]], cyt, fixed = TRUE)) |>
    dplyr::mutate(
      type = "single"
    )
}

# Derive any-positive statistics from combination data.
# Sums over all combinations with at least one positive marker.
#
# Arguments:
#   stats_bs_combn - Tibble with combination-level statistics
#   chnl - Vector of channel/marker names
#   grp - Grouping variables
#   levels - Positive/negative pattern markers (default: c("~+~", "~-~"))
#   resp - Response variable name
#
# Returns:
#   A tibble with any-positive statistics (type = "any")
get_stats_bs_any <- function(stats_bs_combn,
                             chnl,
                             grp = NULL,
                             levels = c("~+~", "~-~"),
                             resp) {
  # Sum over all markers, keeping any combination with at least one positive
  stats_bs_combn |>
    dplyr::filter(grepl(levels[[1]], cyt, fixed = TRUE)) |>
    UtilsCytoRSV::sum_over_markers(
      grp = grp,
      cmbn = "cyt",
      levels = levels,
      markers_to_sum = chnl,
      resp = resp
    ) |>
    dplyr::mutate(
      type = "any"
    )
}

get_stats_tbl_bs_tg <- function(stats_tbl) {
  stats_tbl |>
    dplyr::rename(
      prop_bs_tg = prop_bs
    ) |>
    dplyr::select(
      type, cyt, sample_ind, prop_bs_tg
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sample_ind = as.character(sample_ind)
    )
}