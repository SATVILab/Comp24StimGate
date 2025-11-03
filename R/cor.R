get_corr_by_sim <- function(stats_tbl_bs) {
  stats_tbl_bs |>
    dplyr::group_split(cyt, type, sim) |>
    purrr::map_df(function(x) {
      cyt <- x$cyt[1]
      type <- x$type[1]
      sim <- x$sim[1]
      corr_tbl <- x |>
        dplyr::summarise(
          corr_concordance_stimgate = DescTools::CCC(
            prop_bs_actual, prop_bs_stimgate
          )$rho.c$est,
          corr_pearson_stimgate = cor(
            prop_bs_actual, prop_bs_stimgate, use = "pairwise.complete.obs"
          ),
          corr_spearman_stimgate = cor(
            prop_bs_actual, prop_bs_stimgate, method = "spearman",
            use = "pairwise.complete.obs"
          )
        )
      cn_tg <- colnames(x)[grepl("prop_bs_tg_", colnames(x))]
      for (cn in cn_tg) {
        corr_ccc <- DescTools::CCC(
          x[["prop_bs_actual"]], x[[cn]]
        )$rho.c$est
        corr_pearson <- cor(
          x[["prop_bs_actual"]], x[[cn]], use = "pairwise.complete.obs"
        )
        corr_spearman <- cor(
          x[["prop_bs_actual"]], x[[cn]], method = "spearman",
          use = "pairwise.complete.obs"
        )
        short_nm <- gsub("prop_bs_tg_", "", cn)
        corr_tbl[[paste0("corr_concordance_tg_", short_nm)]] <- corr_ccc
        corr_tbl[[paste0("corr_pearson_tg_", short_nm)]] <- corr_pearson
        corr_tbl[[paste0("corr_spearman_tg_", short_nm)]] <- corr_spearman
      }
      corr_tbl |>
        dplyr::mutate(
          cyt = cyt,
          type = type,
          sim = sim
        ) |>
        dplyr::select(type, cyt, sim, cyt, dplyr::everything())
  })
}

get_corr_tbl_overall <- function(corr_tbl_by_sim) {
  corr_tbl_by_sim |>
    tidyr::pivot_longer(
      -c(type, cyt, sim),
      names_to = "mtd",
      values_to = "corr"
    ) |>
    dplyr::mutate(
      mtd = gsub("corr_", "", mtd),
      mtd = gsub("tg_", "tg", mtd)
    ) |>
    tidyr::separate(
      mtd,
      into = c("corr_method", "gating_method"),
      sep = "_",
      remove = FALSE
    ) |>
    dplyr::select(-mtd) |>
    dplyr::group_by(type, cyt, gating_method, corr_method) |>
    dplyr::summarise(
      corr_mean = mean(corr, na.rm = TRUE),
      corr_sd = sd(corr, na.rm = TRUE),
      corr_lb = quantile(corr, 0.025, na.rm = TRUE),
      corr_ub = quantile(corr, 0.975, na.rm = TRUE)
    )
}