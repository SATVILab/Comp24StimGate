test_that(".plot_adjacent_dist_plot returns ggplot and saves file", {
  tmp <- tempdir()
  n_obs_vec <- c(3, 5, 10)
  probs <- c(0.25, 0.5, 0.75)
  p <- .plot_adjacent_dist_plot(
    n_obs_vec = n_obs_vec,
    n_draws = 500,
    probs = probs,
    rng_seed = 1,
    path_dir_save = NULL
  )
  expect_s3_class(p, "ggplot")
  # data used for plotting contains all requested probs (as labels)
  data_probs <- unique(p$data$prob)
  expect_true(all(probs %in% data_probs))

  # test saving to disk
  file_name <- "test_adjacent_plot.png"
  out_path <- file.path(tmp, file_name)
  if (file.exists(out_path)) file.remove(out_path)
  invisible(.plot_adjacent_dist_plot(
    n_obs_vec = n_obs_vec,
    n_draws = 500,
    probs = probs,
    rng_seed = 1,
    path_dir_save = tmp,
    file_name = file_name
  ))
  expect_true(file.exists(out_path))
  file.remove(out_path)
})
