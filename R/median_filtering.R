#' Median filter a 3D image
#'
#' The function applies a median filter to the 3D image using mmand::medianFilter. 
#'
#' @param roi_mask 3D array of image
#' @param r kernel size for median filter
#' @param ... extra arguments passed to mclapply
#' @return 3D array of median filtered image
#' @noRd
median_filtering_3d <- function(roi_mask, r = 10){
  k <- mmand::shapeKernel(c(r,r,2), type="box")
  roi_mask_filtered <- mmand::medianFilter(roi_mask, k)
  roi_mask_filtered[which(roi_mask_filtered == 0.5, arr.ind = TRUE)] <- 1 # correct cases where median == 0.5
  return(roi_mask_filtered)
}

#' Median filter a 4D image
#'
#' The function applies a median filter to each time point of the 4D image using median_filtering_3d function.
#'
#' @param roi_mask 4D array of image
#' @param r kernel size for median filter
#' @param n.cores number of cores to use for parallel processing
#' @param ... extra arguments passed to mclapply
#' @return 4D array of median filtered image
#' @noRd
median_filtering_4d <- function(roi_mask, r = 10, n.cores=1, ...) {

  dims <- dim(roi_mask)
  T <- dims[4]

  roi_mask_filtered <- parallel::mclapply(1:T, function(t) median_filtering_3d(roi_mask[,,,t], r), mc.cores=n.cores, ...)
  roi_mask_filtered <- list2nifti(roi_mask_filtered, dims)
  return(roi_mask_filtered)
}

#' Median filter an image
#'
#' The function takes an image and applies the appropriate median filter function 
#' (median_filtering_3d or median_filtering_4d) based on the dimensionality of the image.
#'
#' @param roi_mask image to be median filtered (3D or 4D array)
#' @param r kernel size for median filter
#' @param n.cores number of cores
#' @param ... extra arguments passed to mclapply
#' @export median_filtering
median_filtering <- function(roi_mask, r = 10, n.cores=1, ...) {
  dims <- dim(roi_mask)

  if(length(dims) == 3){
    median_filtering_3d(roi_mask, r)
  } else if (length(dims) == 4){
    median_filtering_4d(roi_mask, r, n.cores=n.cores, ...)
  } else {
    if (length(dims) < 3){
      stop('Not enough dimensions! Array must have at least 3 dimensions.')
    }

    if (length(dims) > 4){
      stop('Too many dimensions! Array must have at most 4 dimensions.')
    }
  }
}
