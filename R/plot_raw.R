# Plot raw density distributions of marker expression for stimulated vs unstimulated samples.
# Creates density plots showing the distribution of each marker's expression values,
# faceted by batch, with separate colors for stimulated and unstimulated samples.
#
# Arguments:
#   batch_list - List defining which sample indices belong to each batch
#   chnl_list - List of channel objects containing simulated data
#   fs - flowSet with the simulated expression data (not directly used)
#   path_dir_save - Directory path where plots should be saved
#
# Returns:
#   The path_dir_save directory path (after saving all plots)
plot_raw <- function(batch_list,
                     chnl_list,
                     fs,
                     path_dir_save) {
  # Build a tibble combining all samples across batches
  plot_tbl <- purrr::map_df(seq_along(batch_list), function(i) {
    ind_vec <- batch_list[[i]]
    purrr::map_df(ind_vec, function(ind) {
      # Extract expression data for this sample
      flowCore::exprs(chnl_list[[length(chnl_list)]]$fs[[ind]]) |>
        tibble::as_tibble() |>
        dplyr::select(
          dplyr::all_of(names(chnl_list))
        ) |>
        dplyr::mutate(
          batch = i,
          sample_ind = ind,
          # Last sample in each batch is unstimulated
          stim = if (ind == ind_vec[length(ind_vec)]) "uns" else "stim"
        )
    })
  })
  # Create output directory if needed
  if (!dir.exists(path_dir_save)) {
    dir.create(path_dir_save, recursive = TRUE)
  }
  # Generate and save a plot for each channel
  chnl_vec <- names(chnl_list)
  chnl <- chnl_vec[[1]]
  for (chnl in chnl_vec) {
    p_bc1 <- ggplot(
      plot_tbl, aes(x = .data[[chnl]], fill = stim)
      ) +
      geom_density(
        aes(fill = stim),
        alpha = 0.2, bw = 0.025
      ) +
      facet_wrap(~batch, ncol = 4) 
    # Save plot to PNG file
    ggplot2::ggsave(
      filename = file.path(
        path_dir_save, paste0("p-raw-", chnl, ".png")
      ),
      plot = p_bc1,
      width = 15,
      height = 15,
      units = "cm",
      dpi = 300
    )
  }
  path_dir_save
}