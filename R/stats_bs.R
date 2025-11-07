get_stats_bs_actual <- function(batch_list,
                                chnl_list) {
  stats_tbl_combn <- get_stats_bs_actual_combn(
    batch_list = batch_list,
    chnl_list = chnl_list
  )
  stats_tbl_single <- get_stats_bs_single(
    stats_bs_combn = stats_tbl_combn,
    chnl = names(chnl_list),
    grp = c("type", "sample_ind"),
    resp = "prop_bs_actual"
  )
  stats_tbl_any <- get_stats_bs_any(
    stats_bs_combn = stats_tbl_combn,
    chnl = names(chnl_list),
    grp = c("type", "sample_ind"),
    resp = "prop_bs_actual"
  )
  stats_tbl_any |>
    dplyr::bind_rows(
      stats_tbl_single
    ) |>
    dplyr::bind_rows(stats_tbl_combn) |>
    dplyr::mutate(
      sample_ind = as.character(sample_ind)
    )
}

get_stats_bs_actual_combn <- function(batch_list,
                                      chnl_list) {
  pos_vec_sample <- lapply(batch_list, function(x) x[-length(x)]) |>
    unlist()
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

get_stats_tbl_bs_stimgate <- function(path_project,
                                      chnl) {
  stats_tbl_combn <- get_stats_bs_stimgate_combn(path_project)
  stats_tbl_single <- get_stats_bs_single(
    stats_bs_combn = stats_tbl_combn,
    chnl = chnl,
    grp = c("type", "sample_ind"),
    resp = "prop_bs_stimgate"
  )
  stats_tbl_any <- get_stats_bs_any(
    stats_bs_combn = stats_tbl_combn,
    chnl = chnl,
    grp = c("type", "sample_ind"),
    resp = "prop_bs_stimgate"
  )
  stats_tbl_any |>
    dplyr::bind_rows(
      stats_tbl_single
    ) |>
    dplyr::bind_rows(stats_tbl_combn) |>
    dplyr::mutate(
      sample_ind = as.character(sample_ind)
    )
}

get_stats_bs_stimgate_combn <- function(path_project) {
  stimgate::get_stats(path_project) |>
    dplyr::rename(sample_ind = ind, prop_bs_stimgate = prop_bs) |>
    dplyr::select(
      sample_ind, cyt_combn, prop_bs_stimgate
    ) |>
    dplyr::filter(grepl("~\\+~", cyt_combn)) |>
    dplyr::mutate(
      type = "combn"
    ) |>
    dplyr::rename(
      cyt = cyt_combn
    ) |>
    dplyr::select(type, cyt, sample_ind, prop_bs_stimgate)
}

get_stats_bs_single <- function(stats_bs_combn,
                                chnl,
                                grp = NULL,
                                levels = c("~+~", "~-~"),
                                resp) {
  purrr::map_df(chnl, function(x) {
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
    dplyr::filter(grepl(levels[[1]], cyt, fixed = TRUE)) |>
    dplyr::mutate(
      type = "single"
    )
}

get_stats_bs_any <- function(stats_bs_combn,
                             chnl,
                             grp = NULL,
                             levels = c("~+~", "~-~"),
                             resp) {
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