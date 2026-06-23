#' @title Simulate all cytokine combinations for a set of stimulation conditions for a single biological sample
#' 
#' @description Simulate a set of stimulation conditions for a single biological sample, where the first condition is always the unstimulated condition.
#' 
#' @return A list of length nCondition, where each element is a list with two elements: conditionMatrix and conditionLabels. conditionMatrix is a matrix of simulated data for that condition, and conditionLabels is a vector of labels for each cell in that condition. The first element is named `unstim`, and subsequent elements are named `stim1`, `stim2`, etc.
#' @internal
simCytSample <- function(
  # first sample is always the unstimulated sample
  nMarker,
  # number of samples
  nCondition,
  # number of clusters,
  nCluster,
  nCellByCondition, # vector of number of cells by sample,
  # where first sample is the unstim.
  # if a single number, then used for all
  transformationFunc,
  mixtureType = "gaussianOnly",
  sampleMeanMat = NA,
  fixedLabelVec = NA,
  probVecUns, # probability vector for sampling from unstim
  effectSizeAddVecList = NULL, # list where each element
  # is for a stimulated sample, 
  # and where it is the absolute increase in the proportion of 
  # cells in the cytokine-positive population
  effectSizeMultVecList = NULL, # same as above, but multiplicative
  probResponseVecByCondition = NULL, # list where it's the sampling vector for each
  # stimulation condition
  conditionPerturbationSd = 0,
  conditionClusterPerturbationSd = 0
) {
  # check inputs
  stopifnot(is.integer(nCondition))
  stopifnot(is.integer(nMarker))
  stopifnot(nCondition > 1L)
  stopifnot(is.integer(nCluster))
  stopifnot(nCluster > 0L)
  stopifnot(nCluster %in% 2^(1:10))
  # check transformation function is a function
  stopifnot(is.function(transformationFunc))
  # probResponseVecByCondition
  stopifnot(is.list(probResponseVecByCondition))
  stopifnot(all(sapply(probResponseVecByCondition, is.numeric)))
  stopifnot(length(probResponseVecByCondition) == (nCondition - 1L))
  stopifnot(identical(c(sapply(probResponseVecByCondition, length), nCluster)))
  # probVecUns
  stopifnot(is.numeric(probVecUns))
  stopifnot(length(probVecUns) == nCluster)
  stopifnot(all(probVecUns >= 0))
  stopifnot(all(probVecUns <= 1))
  stopifnot(abs(sum(probVecUns) - 1) < 1e-6)
  # conditionPerturbationSd
  stopifnot(is.numeric(conditionPerturbationSd))
  stopifnot(length(conditionPerturbationSd) == 1L)
  stopifnot(conditionPerturbationSd >= 0)
  # conditionClusterPerturbationSd
  stopifnot(is.numeric(conditionClusterPerturbationSd))
  stopifnot(length(conditionClusterPerturbationSd) == 1L)
  stopifnot(conditionClusterPerturbationSd >= 0)
  # sampleMeanMat
  stopifnot(is.matrix(sampleMeanMat))
  stopifnot(nrow(sampleMeanMat) == nCluster)
  stopifnot(ncol(sampleMeanMat) == nMarker)
  stopifnot(!any(is.na(sampleMeanMat)))
  stopifnot(is.numeric(sampleMeanMat))
  stopifnot(length(fixedLabelVec) == nCluster)
  nCellByCondition <- if (length(nCellByCondition) == 1L) {
    rep(nCellByCondition, nCondition)
  } else {
    nCellByCondition
  }
  if (!is.null(probVecUns) && !is.null(probVecBySample)) {
    stop("Cannot specify both probVecUns and probVecBySample at the same time")
  }
  # okay, so now we have to work out the actual proportions of each
  # cluster by samples
  probVecByCondition <- list(probVecUns)
  if (!is.null(probResponseVecByCondition)) {
    probVecByCondition <- probVecByCondition |>
      append(lapply(probResponseVecByCondition, function(probResponseVec) {
        probVecUns + probResponseVec
      }))
  }

  # now we can simulate each condition
  conditionList <- lapply(seq_len(nCondition), function(i) {
    simCytCondition(
      nMarker = nMarker,
      nCell = nCellByCondition[[i]],
      transformationFunc = transformationFunc,
      mixtureType = mixtureType,
      sampleMeanMat = sampleMeanMat,
      fixedLabelVec = fixedLabelVec,
      probVecSample = probVecByCondition[[i]],
      conditionPerturbationSd = conditionPerturbationSd,
      conditionClusterPerturbationSd = conditionClusterPerturbationSd
    )
  }) |>
    stats::setNames(c("unstim", paste0("stim", seq_len(nCondition - 1L))))
}

simCytCondition <- function(
  # analagous to simSample
  nMarker,
  nCell,
  transformationFunc,
  mixtureType = "gaussianOnly",
  sampleMeanMat = NA,
  fixedLabelVec = NA,
  # noiseDim = 0
  probVecSample,
  # isKnockout = FALSE,
  # isSpikeIn = FALSE,
  # sRegime = 0,
  # targetRanks = c(length(probVecSample) - 1, length(probVecSample)),
  conditionPerturbationSd = 0,
  conditionClusterPerturbationSd = 0
  # batchEffect = 0
) {
  numClusters <- nrow(sampleMeanMat)
  probVec <- probVecSample
  # skipped knockout and spike-in here

  # get number of cells per population
  nCellVec <- as.vector(t(stats::rmultinom(1, nCell, probVec)))
  nCellVecCum <- cumsum(nCellVec)

  # get labels of cells
  nCellVecObserved <- nCellVec[nCellVec > 0L]
  fixedLabelVecObserved <- fixedLabelVec[nCellVec > 0L]
  cellLabelVec <- lapply(seq_along(nCellVecObserved), function(i) {
    rep(fixedLabelVecObserved[i], nCellVecObserved[i])
  }) |> unlist()

  # population matrix
  outData <- matrix(NA_integer_, nrow = nCell, ncol = nMarker)
  for (clusterNumber in seq_len(numClusters)) {
    nCell <- nCellVec[[clusterNumber]]
    # skip if there aren't any cells to simulate here
    if (nCell == 0L) {
      next
    }
    # get indicies to insert into outData
    outDataIndClusterLower <- if (clusterNumber == 1L) {
      1L
    } else {
      nCellVecCum[clusterNumber - 1] + 1
    }
    outDataIndClusterUpper <- nCellVecCum[[clusterNumber]]
    outDataIndClusterVec <- seq.int(
      outDataIndClusterLower,
      outDataIndClusterUpper
    )
    sampleMeanVecCluster <- as.numeric(sampleMeanMat[clusterNumber, , drop = TRUE])
    outData[outDataIndClusterVec, ] <- simCytConditionCluster(
      nMarker = nMarker,
      nCell = nCell,
      sampleMeanVecCluster = sampleMeanVecCluster,
      perturbationSd = conditionPerturbationSd,
      mixtureType = mixtureType
    )
  }
  for (i in seq_len(nMarker)) outData[, i] <- transformationFunc(outData[, i])
  colnames(outData) <- paste0("F", seq_len(nMarker))
  reorder_vec <- sample.int(nrow(outData))
  outData <- outData[reorder_vec, ]
  cellLabelVec <- cellLabelVec[reorder_vec]
  list(
    conditionMatrix = outData,
    conditionLabels = cellLabelVec#,
    # knockoutInfo = knockoutStatus
  )
}

simCytConditionCluster <- function(
  nMarker,
  nCell,
  sampleMeanVecCluster,
  perturbationSd = 0,
  mixtureType
) {
  conditionPerturbationVec <- if (perturbationSd == 0L) {
    0
  } else {
    rnorm(nMarker, mean = 0, sd = perturbationSd)
  }
  conditionMeanVecCluster <- sampleMeanVecCluster + conditionPerturbationVec
  currentSigma <- Posdef(nMarker)
  simCytConditionClusterData(
    mixtureType = mixtureType,
    clusterNumber = clusterNumber,
    nCell = nCell,
    muVec = conditionMeanVecCluster,
    sigmaMat = currentSigma
  )
}

simCytConditionClusterData <- function(
  mixtureType,
  clusterNumber,
  nCell,
  muVec,
  sigmaMat
) {
  if (mixtureType == "tPlusGauss") {
    if ((clusterNumber %% 2) == 0) {
      MASS::mvrnorm(
        nCell,
        mu = muVec,
        Sigma = sigmaMat
      )
    } else {
      mvtnorm::rmvt(
        nCell,
        delta = muVec,
        sigma = sigmaMat,
        df = 2
      )
    }
  } else if (mixtureType == "tOnly") {
    mvtnorm::rmvt(
      nCell,
      delta = muVec,
      sigma = sigmaMat,
      df = 2
    )
  } else {
    MASS::mvrnorm(
      nCell,
      mu = muVec,
      Sigma = sigmaMat
    )
  }
}
