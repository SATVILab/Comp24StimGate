#' Download La Manno Brain Data (Standalone)
#'
#' Downloads the La Manno brain scRNA-seq data directly from the gypsum backend
#' without requiring the scRNAseq package.
#'
#' @param which Character string specifying which dataset to download. 
#'   One of:  "human-es", "human-embryo", "human-ips", "mouse-adult", "mouse-embryo"
#' @param cache Directory to cache downloaded files.  Default uses gypsum's cache.
#' @param overwrite Logical, whether to overwrite cached files. 
#'
#' @return A SingleCellExperiment object
#'
#' @examples
#' # Download human embryonic stem cell data
#' sce <- downloadLaMannoBrainData("human-es")
#'
#' @export
downloadLaMannoBrainData <- function(
    which = c("human-es", "human-embryo", "human-ips", "mouse-adult", "mouse-embryo"),
    cache = gypsum::cacheDirectory(),
    overwrite = FALSE
) {
  # Validate input
  which <- match.arg(which)
  
  # Required packages
  if (!requireNamespace("gypsum", quietly = TRUE)) {
    stop("Package 'gypsum' is required.  Install with: BiocManager::install('gypsum')")
  }
  if (!requireNamespace("alabaster.base", quietly = TRUE)) {
    stop("Package 'alabaster.base' is required. Install with: BiocManager::install('alabaster.base')")
  }
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    stop("Package 'SingleCellExperiment' is required. Install with: BiocManager::install('SingleCellExperiment')")
  }
  if (!requireNamespace("alabaster.sce", quietly = TRUE)) {
    stop("Package 'alabaster.sce' is required. Install with: BiocManager::install('alabaster.sce')")
  }
  
  # Dataset parameters
  package <- "scRNAseq"
  name <- "lamanno-brain-2016"
  version <- "2023-12-17"
  path <- which
  
  # Download the dataset version to cache
  version_path <- gypsum::saveVersion(
    project = package,
    asset = name,
    version = version,
    cache = cache,
    overwrite = overwrite
  )
  
  # Construct path to the specific subdataset
  obj_path <- file.path(version_path, path)
  
  # Read the object using alabaster
  sce <- alabaster.base::readObject(obj_path)
  
  # Realize assays (convert ReloadedArray to standard matrices)
  sce <- .realize_assays(sce)
  
  # Set column names from Cell_ID column in colData
  cell_id_col <- grep("Cell_ID", colnames(SummarizedExperiment::colData(sce)), 
                      ignore.case = TRUE, value = TRUE)
  if (length(cell_id_col) > 0) {
    colnames(sce) <- SummarizedExperiment::colData(sce)[[cell_id_col[1]]]
  }
  
  return(sce)
}

#' Helper function to realize assays
#' @noRd
.realize_assays <- function(sce) {
  for (assay_name in SummarizedExperiment::assayNames(sce)) {
    mat <- SummarizedExperiment::assay(sce, assay_name, withDimnames = FALSE)
    
    # Check if it's a ReloadedArray and convert
    if (methods::is(mat, "ReloadedArray")) {
      if (DelayedArray::is_sparse(mat)) {
        if (DelayedArray::type(mat) == "logical") {
          mat <- methods::as(mat, "lgCMatrix")
        } else {
          mat <- methods::as(mat, "dgCMatrix")
        }
      } else {
        mat <- as.array(mat)
      }
      SummarizedExperiment::assay(sce, assay_name, withDimnames = FALSE) <- mat
    }
  }
  return(sce)
}


# Minimal version without realize (if you want the DelayedArray version)
downloadLaMannoBrainDataMinimal <- function(
    which = c("human-es", "human-embryo", "human-ips", "mouse-adult", "mouse-embryo"),
    cache = gypsum:: cacheDirectory(),
    overwrite = FALSE
) {
  which <- match.arg(which)
  
  version_path <- gypsum::saveVersion(
    project = "scRNAseq",
    asset = "lamanno-brain-2016",
    version = "2023-12-17",
    cache = cache,
    overwrite = overwrite
  )
  
  obj_path <- file.path(version_path, which)
  sce <- alabaster.base::readObject(obj_path)
  
  # Set cell IDs as column names
  cell_id_col <- grep("Cell_ID", colnames(SummarizedExperiment::colData(sce)), 
                      ignore.case = TRUE, value = TRUE)
  if (length(cell_id_col) > 0) {
    colnames(sce) <- SummarizedExperiment:: colData(sce)[[cell_id_col[1]]]
  }
  
  return(sce)
}