get_fs <- function() {
  path_hub <- file.path(Sys.getenv("HOME"), ".cache/R/ExperimentHub")
  if (!dir.exists(path_hub)) {
    dir.create(path_hub, recursive = TRUE)
  }
  HDCytoData::Bodenmiller_BCR_XL_flowSet()
}