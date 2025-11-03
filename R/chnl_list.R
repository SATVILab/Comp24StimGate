get_chnl_list <- function(fs) {
  batch_list <- lapply(1:8, function(i) seq((i - 1) * 2 + 1, i * 2))
  chnl_vec <- c("BC1(La139)Dd", "BC2(Pr141)Dd")
  args_list_bc1  <- list(
    batch_list = batch_list,
    chnl = "BC1(La139)Dd",
    prop_mean_pos = 0.05,
    prop_sd_pos = 0.01,
    prop_mean_neg = 0.005,
    prop_sd_neg = 0.0075,
    expr_mean_neg = 0,
    expr_mean_pos = 2,
    expr_sd_pos = 0.1
  )
  args_list_bc2 <- list(
    batch_list = batch_list,
    chnl = "BC2(Pr141)Dd",
    prop_mean_pos = 0.01,
    prop_sd_pos = 0.01, 
    prop_mean_neg = 0.005,
    prop_sd_neg = 0.0075,
    expr_mean_neg = 0,
    expr_mean_pos = 2,
    expr_sd_pos = 0.1
  )
  args_list <- list(
    "BC1(La139)Dd" = args_list_bc1, "BC2(Pr141)Dd" = args_list_bc2
  )

  sample_chnls(args_list = args_list, fs = fs)
}

get_chnl_list_easy <- function(fs) {
  batch_list <- lapply(1:8, function(i) seq((i - 1) * 2 + 1, i * 2))
  chnl_vec <- c("BC1(La139)Dd", "BC2(Pr141)Dd")
  args_list_bc1  <- list(
    batch_list = batch_list,
    chnl = "BC1(La139)Dd",
    prop_mean_pos = 0.05,
    prop_sd_pos = 0.01,
    prop_mean_neg = 0.005,
    prop_sd_neg = 0.0075,
    expr_mean_neg = 0,
    expr_mean_pos = 2,
    expr_sd_pos = 0.1
  )
  args_list_bc2 <- list(
    batch_list = batch_list,
    chnl = "BC2(Pr141)Dd",
    prop_mean_pos = 0.01,
    prop_sd_pos = 0.01, 
    prop_mean_neg = 0.005,
    prop_sd_neg = 0.0075,
    expr_mean_neg = 0,
    expr_mean_pos = 2,
    expr_sd_pos = 0.1
  )
  args_list <- list(
    "BC1(La139)Dd" = args_list_bc1, "BC2(Pr141)Dd" = args_list_bc2
  )

  sample_chnls(args_list = args_list, fs = fs)
}

get_chnl_list_poor_separation <- function(fs) {
  batch_list <- lapply(1:8, function(i) seq((i - 1) * 2 + 1, i * 2))
  chnl_vec <- c("BC1(La139)Dd", "BC2(Pr141)Dd")
  args_list_bc1  <- list(
    batch_list = batch_list,
    chnl = "BC1(La139)Dd",
    prop_mean_pos = 0.05,
    prop_sd_pos = 0.01,
    prop_mean_neg = 0.005,
    prop_sd_neg = 0.0075,
    expr_mean_neg = 0,
    expr_mean_pos = 0.5,
    expr_sd_pos = 0.1
  )
  args_list_bc2 <- list(
    batch_list = batch_list,
    chnl = "BC2(Pr141)Dd",
    prop_mean_pos = 0.01,
    prop_sd_pos = 0.01,
    prop_mean_neg = 0.005,
    prop_sd_neg = 0.0075,
    expr_mean_neg = 0,
    expr_mean_pos = 0.5,
    expr_sd_pos = 0.2
  )
  args_list <- list(
    "BC1(La139)Dd" = args_list_bc1, "BC2(Pr141)Dd" = args_list_bc2
  )

  sample_chnls(args_list = args_list, fs = fs)
}
