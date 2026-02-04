test_that(".plot_std_normal_density returns ggplot and uses requested y breaks", {
  break_x <- c(0.05, 0.2, 0.5, 1, 2, 2.5)
  decimals <- 3L

  p <- .plot_std_normal_density(break_x = break_x, decimals = decimals, path_dir_save = NULL)
  expect_s3_class(p, "ggplot")

  gb <- ggplot2::ggplot_build(p)
  # extract computed y breaks from ggplot build (panel_params structure)
  y_breaks_plot <- gb$layout$panel_params[[1]]$y$breaks
  expected <- sort(round(stats::dnorm(break_x), decimals))
  expect_equal(as.numeric(y_breaks_plot), as.numeric(expected))

  # ensure we added label points and ggrepel labels (GeomLabelRepel)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("GeomLabelRepel", geoms)))
  expect_true(any(grepl("GeomPoint", geoms)))
})

test_that(".plot_std_normal_density saves file when path provided", {
  tmp <- tempdir()
  file_name <- "std_normal_test.png"
  path_p <- file.path(tmp, file_name)
  if (file.exists(path_p)) file.remove(path_p)
  out <- invisible(.plot_std_normal_density(path_dir_save = tmp, file_name = file_name))
  expect_true(file.exists(path_p))
  file.remove(path_p)
})
