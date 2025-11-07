# Load a small flowSet from the HDCytoData package for testing and simulation.
# This function downloads the Bodenmiller BCR-XL dataset, which is used as a 
# template for creating simulated flow cytometry data.
#
# Returns:
#   A flowSet object containing the Bodenmiller_BCR_XL dataset
get_fs <- function() {
  # Create ExperimentHub cache directory if it doesn't exist
  path_hub <- file.path(Sys.getenv("HOME"), ".cache/R/ExperimentHub")
  if (!dir.exists(path_hub)) {
    dir.create(path_hub, recursive = TRUE)
  }
  # Download and return the flowSet
  HDCytoData::Bodenmiller_BCR_XL_flowSet()
}