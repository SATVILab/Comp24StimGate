get_stats_tbl_tg <- function(gate_tbl,
                             chnl_list,
                             batch_list,
                             path_gs) {
  stats_tbl_single <- get_stats_tg_single(
    gate_tbl = gate_tbl,
    chnl_list = chnl_list,
    batch_list = batch_list,
    path_gs = path_gs
  )
  stats_tbl_combn <- get_stats_combn(stats_tbl_single)
  stats_tbl_any <- get_stats_tg_any(stats_tbl_combn)
  stats_tbl_any |>
    dplyr::bind_rows(
      stats_tbl_single |>
        dplyr::mutate(
          cyt = paste0(cyt, "~+~")
        )
    ) |>
    dplyr::bind_rows(stats_tbl_combn) |>
    dplyr::mutate(sample_ind = as.character(sample_ind))
}

get_stats_tg_single <- function(gate_tbl, chnl_list, batch_list, path_gs) {
  gs <- suppressMessages(flowWorkspace::load_gs(path_gs))
  purrr::map_df(names(chnl_list), function(chnl) {
    purrr::map_df(batch_list, function(batch) {
      ind_uns <- batch[[length(batch)]]
      expr_tbl_uns <- flowWorkspace::gh_pop_get_data(gs[[ind_uns]]) |>
        flowCore::exprs() |>
        tibble::as_tibble()
      purrr::map_df(seq_along(batch[-length(batch)]), function(i) {
        ind_stim <- batch[[i]]
        expr_tbl_stim <- flowWorkspace::gh_pop_get_data(gs[[ind_stim]]) |>
          flowCore::exprs() |>
          tibble::as_tibble()
        gate <- gate_tbl$gate[
          gate_tbl$ind == ind_stim & gate_tbl$chnl == chnl
        ]
        ind_pos_stim <- which(expr_tbl_stim[[chnl]] > gate)
        ind_pos_uns <- which(expr_tbl_uns[[chnl]] > gate)
        tibble::tibble(
          batch = batch[[i]],
          chnl = chnl,
          ind_stim = ind_stim,
          ind_uns = ind_uns,
          n_total_stim = nrow(expr_tbl_stim),
          n_total_uns = nrow(expr_tbl_uns),
          n_pos_stim = length(ind_pos_stim),
          n_pos_uns = length(ind_pos_uns),
          ind_pos_stim = list(ind_pos_stim),
          ind_pos_uns = list(ind_pos_uns)
        ) |>
          dplyr::mutate(
            prop_stim = n_pos_stim / n_total_stim,
            prop_uns = n_pos_uns / n_total_uns,
            prop_bs = prop_stim - prop_uns,
            freq_bs = prop_bs * 1e2
          )
      })
    })
  }) |>
    dplyr::mutate(
      type = "single"
    ) |>
    dplyr::rename(cyt = chnl, sample_ind = ind_stim) |>
    dplyr::select(
      type, cyt, dplyr::everything()
    )
}

get_stats_combn <- function(stats_tbl_single) {
  chnl_vec <- .get_chnl_vec(stats_tbl_single)
  opt_list <- .get_opts(chnl_vec)
  combn_vec <- .get_combn(chnl_vec, opt_list)
  stats_tbl_single |>
    dplyr::group_split(batch) |>
    purrr::map_df(function(x) {
      # for each channel, we need to get the
      # indices that are positive for stim and unstim
      purrr::map_df(seq_along(opt_list), function(i) {
        opt <- opt_list[[i]]
        ind_stim <- lapply(seq_along(opt), function(j) {
          opt_chnl <- opt[j]
          if (opt_chnl == "~+~") {
            x$ind_pos_stim[[j]]
          } else if (opt_chnl == "~-~") {
            setdiff(seq_len(x$n_total_stim[[j]]), x$ind_pos_stim[[j]])
          } else {
            stop("Unknown option")
          }
        }) |>
          Reduce(f = intersect)
        ind_uns <- lapply(seq_along(opt), function(j) {
          opt_chnl <- opt[j]
          if (opt_chnl == "~+~") {
            x$ind_pos_uns[[j]]
          } else if (opt_chnl == "~-~") {
            setdiff(seq_len(x$n_total_uns[[j]]), x$ind_pos_uns[[j]])
          } else {
            stop("Unknown option")
          }
        }) |>
          Reduce(f = intersect)
        x[1, ] |>
          dplyr::select(-c(cyt, n_pos_stim:freq_bs)) |>
          dplyr::mutate(
            cyt_combn =  combn_vec[[i]],
            n_pos_stim = length(ind_stim),
            n_pos_uns = length(ind_uns),
            ind_pos_stim = list(ind_stim),
            ind_pos_uns = list(ind_uns)
          ) |>
          dplyr::mutate(
            prop_stim = n_pos_stim / n_total_stim,
            prop_uns = n_pos_uns / n_total_uns,
            prop_bs = prop_stim - prop_uns,
            freq_bs = prop_bs * 1e2
          ) |>
          dplyr::select(
            sample_ind, cyt_combn, batch, prop_bs, dplyr::everything()
          ) |>
          dplyr::rename(
            cyt = cyt_combn
          ) |>
          dplyr::mutate(
            type = "combn"
          )
      })
    }) |>
    dplyr::filter(!cyt == combn_vec[[length(combn_vec)]])
}

.get_chnl_vec <- function(x) {
  unique(c(x[["cyt"]], x[["cyt"]], x[["cyt_combn"]])) |>
    as.character()
}

.get_opts <- function(chnl) {
  opt_list <- list("~+~", "~-~")
  for (i in seq_along(chnl[-1])) {
    opt_list_new <- list()
    for (j in seq_along(opt_list)) {
      opt_list_new <- opt_list_new |>
        append(list(
          c(opt_list[[j]], "~+~"),
          c(opt_list[[j]], "~-~")
        ))
    }
    opt_list <- opt_list_new
  }
  opt_list
}

.get_combn <- function(chnl_vec, opt_list) {
  purrr::map_chr(opt_list, function(x) {
    paste0(chnl_vec, x) |> paste0(collapse = "")
  })
}

get_stats_tg_any <- function(stats_tbl_combn) {
  stats_tbl_combn |>
    dplyr::group_split(
      sample_ind, batch, type, n_total_uns, n_total_stim, ind_uns
    ) |>
    purrr::map_df(function(x) {
      x |>
        dplyr::mutate(
          cyt = "",
          type = "any"
        ) |>
        dplyr::group_by(
          sample_ind, cyt, type, n_total_uns, n_total_stim, ind_uns, batch
        ) |>
        dplyr::summarise(
          prop_bs = sum(prop_bs),
          n_pos_stim = sum(n_pos_stim),
          n_pos_uns = sum(n_pos_uns),
          ind_pos_stim = list(
            Reduce(f = union, x$ind_pos_stim)
          ),
          ind_pos_uns = list(
            Reduce(f = union, x$ind_pos_uns)
          ),
          prop_stim = sum(prop_stim),
          prop_uns = sum(prop_uns),
          freq_bs = sum(freq_bs),
          .groups = "drop"
        )
    }) |>
      dplyr::mutate(type = "any")
}