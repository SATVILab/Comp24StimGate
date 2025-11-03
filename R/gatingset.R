get_gatingset <- function(fs,
                          dir_cache) {
  frames_list <- lapply(seq_along(fs), function(i) fs[[i]])
  fs2 <- flowCore::flowSet(frames = frames_list)
  gs <- flowWorkspace::GatingSet(fs2)
  path_save <- file.path(dir_cache, "gs")
  flowWorkspace::save_gs(
    gs,
    path = path_save
  )
  path_save
}

get_gatingset_from_scratch <- function (dir_cache, scenario) {
  fs <- get_fs()
  chnl_list <- switch(scenario,
    "easy" = get_chnl_list_easy(fs = fs),
    "poor_separation" = get_chnl_list_poor_separation(fs = fs),
    stop("Unknown scenario: ", scenario)
  ) 
  fs_gate <- chnl_list[[length(chnl_list)]]$fs
  if (dir.exists(dir_cache)) {
    unlink(dir_cache, recursive = TRUE)
  }
  dir.create(dir_cache, recursive = TRUE)
  path_gs <- get_gatingset(
    fs = fs_gate,
    dir_cache = dir_cache
  )
  list(
    chnl_list = chnl_list,
    batch_list = chnl_list[[1]]$batch_list,
    marker = names(chnl_list),
    path_gs = path_gs
  )
} 