
n(roi_mask, r = 10){
  k <- mmand::shapeKernel(c(r,r,2), type="box")
  roi_mask_filtered <- mmand::medianFilter(roi_mask, k)
  return(roi_mask_filtered)
}

median.filtering.4d <- function(roi_mask, r = 10, n.cores=1, ...) {

  dims <- dim(roi_mask)
  T <- dims[4]

  roi_mask_filtered <- parallel::mclapply(1:T, function(t) median.filtering.3d(roi_mask[,,,t], r), mc.cores=n.cores, ...)
  roi_mask_filtered <- list2nifti(roi_mask_filtered, dims)
  return(roi_mask_filtered)
}

median.filtering <- function(roi_mask, r = 10, n.cores=1, ...) {
  dims <- dim(roi_mask)

  if(length(dims) == 3){
    median.filtering.3d(roi_mask, r)
  } else if (length(dims) == 4){
    median.filtering.4d(roi_mask, r, n.cores=n.cores, ...)
  } else {
    if (length(dims) < 3){
      stop('Not enough dimensions! Array must have at least 3 dimensions.')
    }

    if (length(dims) > 4){
      stop('Too many dimensions! Array must have at most 4 dimensions.')
    }
  }
}

