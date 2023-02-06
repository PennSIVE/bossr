#' Threshold a 2D image
#'
#' The function applies a threshold to the 2D image and creates a binary mask where all the pixels 
#' with values greater than the threshold are set to 1 and the rest are set to 0.
#'
#' @param img 2D array of image
#' @param thr threshold value
#' @return binary mask where pixels with values greater than the threshold are set to 1 and all others are set to 0
#' @noRd
threshold_2d <- function(img, thr){
    roi_mask <- (img > thr) |> apply(2, as.numeric)
    roi_mask[is.na(roi_mask)] <- 0 # override NA (cause: 0-everywhere slice)
    return(roi_mask)
}

#' Threshold a 3D image
#'
#' The function applies a threshold to each slice of the 3D image using threshold_2d function, 
#' creating a 3D binary mask where all the pixels with values greater than the threshold in each slice are set to 1 and the rest are set to 0.
#'
#' @param img 3D array of image
#' @param thr vector of threshold values (one for each slice of the image)
#' @param n.cores number of cores to use for parallel processing
#' @return 3D binary mask
#' @noRd
threshold_3d <- function(img, thr, n.cores=1, ...){
    dims = dim(img)
    Z = dims[3]
    mask <- 1:Z |> parallel::mclapply(function(z) threshold_2d(img[,,z], thr[z]), 
                    mc.cores=n.cores, ...) |> list2nifti(dims)
    return(mask)
}

#' Threshold a 4D image
#'
#' The function applies a threshold to each slice and time point of the 4D image using threshold_2d function, 
#' creating a 4D binary mask where all the pixels with values greater than the threshold in each slice and time point are set to 1 and the rest are set to 0.
#'
#' @param img 4D array of image
#' @param thr matrix of threshold values (one for each slice and time point of the image)
#' @param n.cores number of cores to use for parallel processing
#' @return 4D binary mask
#' @noRd
threshold_4d <- function(img, thr, n.cores=1, ...){
    dims <- dim(img)
    Z <- dims[3]
    T <- dims[4]
    inputs <- expand.grid(1:Z, 1:T) # split by rows

    threshold_from_inputs <- function(i){
              z <- inputs[i, 1]
              t <- inputs[i, 2]

              threshold_2d(img[,,z,t], thr[z,t])
    }
    mask <- parallel::mclapply(1:nrow(inputs), 
                              threshold_from_inputs,
                               mc.cores = n.cores, 
                               ...) |> 
                    list2nifti(dims)
    return(mask)
}

#' Threshold an image
#'
#' The function takes an image and a threshold value and applies the appropriate threshold function 
#' (threshold_2d, threshold_3d, or threshold_4d) based on the dimensionality of the image. 
#' It also checks that the threshold value has the correct dimensionality and length to match the image.
#'
#' @param img image to be thresholded (2D, 3D, or 4D array)
#' @param thr threshold value (scalar, vector, or matrix)
#' @param n.cores number of cores to use for parallel processing (only used in threshold_3d and threshold_4d)
#' @return binary mask of thresholded image (same dimensionality as input image)
#' @export
threshold_img <- function(img, thr, n.cores=1, ...){
  ndim.img <- length(dim(img))
  # assign dimensions if thr is a vector
  ndim.thr <- ifelse(is.null(dim(thr)), 
                     ifelse(length(thr) == 1, 
                            0, 
                            1),
                    length(dim(thr)))

  # convert threshold matrix to vector if dimensions allow
  # if (is.matrix(thr) & ndim.img <= 2) if (dim(thr)[2] == 1) thr = as.vector(thr) 

  if (ndim.img == 2){ # img is a slice/matrix, thr should be one number
      if (ndim.thr != 0){
        stop("Error in threshold dimensions. Image is a slice but the threshold is not a vector")
      } else if (length(thr) != 1){
        stop("Error in threshold length. Image is a slice but threshold is not a vector of length one")
      } 

      threshold_2d(img, thr)

  } else if (ndim.img == 3){ # img is volume, thr should be vector
    if (ndim.thr != 1){
        stop("Error in threshold dimensions. Image is a volume but the threhold is not a vector")
    } else if (length(thr) != dim(img)[3]){
        stop("Error in threshold length. Threshold length does not match 3rd dimension of image")
    } 
      threshold_3d(img, thr, n.cores=n.cores, ...)

  } else if (ndim.img == 4){ # img is 4D array, thr should be a matrix
      if(ndim.thr != 2){
        stop("Error in threshold dimensions. Image is a 4D array; Threshold should be a matrix")
      } else if (dim(img)[3] != dim(thr)[1]){
        stop("Error in dimension lengths. Dimension Z of image must match 1st dimension of threshold")
      } else if (dim(img)[4] != dim(thr)[2]){ 
        stop("Error in dimension lengths. Dimension T of image must match 2nd dimension of threshold")
      } 

      threshold_4d(img, thr, n.cores=n.cores, ...)
  }
}
