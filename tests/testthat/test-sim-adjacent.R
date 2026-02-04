test_that("input validation works", {
  expect_error(.sim_adjacent_dist_quantiles(1), "must be >= 2")
  expect_error(.sim_adjacent_dist_quantiles(3, n_draws = 0), "must be a single integer >= 1")
  expect_error(.sim_adjacent_dist_quantiles(3, probs = c(-0.1, 0.5)), "probs must be between 0 and 1")
})

test_that("output shape and monotonicity", {
  res <- .sim_adjacent_dist_quantiles(3, n_draws = 1000, probs = c(0.25, 0.5, 0.75, 0.9, 0.95), rng_seed = 1)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5) # single n_obs -> length(probs) rows
  expect_true(all(res$value > 0)) # distances should be positive
  expect_true(isTRUE(all(diff(res$value) >= 0))) # quantiles non-decreasing
})

test_that("works for multiple n_obs and reproducible with seed", {
  a <- .sim_adjacent_dist_quantiles(c(3, 5), n_draws = 500, probs = c(0.25, 0.5), rng_seed = 42)
  b <- .sim_adjacent_dist_quantiles(c(3, 5), n_draws = 500, probs = c(0.25, 0.5), rng_seed = 42)
  expect_equal(a, b) # reproducible
  expect_equal(nrow(a), 2 * 2) # two n_obs * two probs
})
