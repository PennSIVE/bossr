library(argparser)

# Functions
list2nifti <- function(list, dims) list |> unlist() |> array(dims) |> oro.nifti::as.nifti() 

threshold.2d <- function(img, thr){
    roi_mask <- (img < thr) |> apply(2, as.numeric)
    roi_mask[is.na(roi_mask)] <- 0 # override NA (cause: 0-everywhere slice)
    return(roi_mask)
}

threshold.3d <- function(img, thr, n.cores=1, ...){
    dims = dim(img)
    Z = dims[3]
    mask <- 1:Z |> parallel::mclapply(function(z) threshold.2d(img[,,z], thr[z]), 
                    mc.cores=n.cores, ...) |> list2nifti(dims)
    return(mask)
}

threshold.4d <- function(img, thr, n.cores=1, ...){
    dims <- dim(img)
    Z <- dims[3]
    T <- dims[4]
    inputs <- expand.grid(1:Z, 1:T) # split by rows

    threshold_from_inputs <- function(i){
              z <- inputs[i, 1]
              t <- inputs[i, 2]

              threshold.2d(img[,,z,t], thr[z,t])
    }
    mask <- parallel::mclapply(1:nrow(inputs), 
                              threshold_from_inputs,
                               mc.cores = n.cores, 
                               ...) |> 
                    list2nifti(dims)
    return(mask)
}

threshold.img <- function(img, thr, n.cores=1, ...){
  ndim.img <- length(dim(img))
  # assign dimensions if thr is a vector
  ndim.thr <- ifelse(is.null(dim(thr)), 
                     ifelse(length(thr) == 1, 
                            0, 
                            1),
                    length(dim(thr)))

  # convert threshold matrix to vector if dimensions allow
  #if (is.matrix(thr) & ndim.img <= 2) if (dim(thr)[2] == 1) thr = as.vector(thr) 

  if (ndim.img == 2){ # img is a slice/matrix, thr should be one number
      if (ndim.thr != 0){
        stop("Error in threshold dimensions. Image is a slice but the threshold is not a vector")
      } else if (length(thr) != 1){
        stop("Error in threshold length. Image is a slice but threshold is not a vector of length one")
      } 

      threshold.2d(img, thr)

  } else if (ndim.img == 3){ # img is volume, thr should be vector
    if (ndim.thr != 1){
        stop("Error in threshold dimensions. Image is a volume but the threhold is not a vector")
    } else if (length(thr) != dim(img)[3]){
        stop("Error in threshold length. Threshold length does not match 3rd dimension of image")
    } 
      threshold.3d(img, thr, n.cores=n.cores, ...)

  } else if (ndim.img == 4){ # img is 4D array, thr should be a matrix
      if(ndim.thr != 2){
        stop("Error in threshold dimensions. Image is a 4D array; Threshold should be a matrix")
      } else if (dim(img)[3] != dim(thr)[1]){
        stop("Error in dimension lengths. Dimension Z of image must match 1st dimension of threshold")
      } else if (dim(img)[4] != dim(thr)[2]){ 
        stop("Error in dimension lengths. Dimension T of image must match 2nd dimension of threshold")
      } 

      threshold.4d(img, thr, n.cores=n.cores, ...)
  }
}

# Run 
p <- arg_parser("Run intensity modeling for function profiling")
p <- add_argument(p, "--run2d", flag=TRUE, help = 'Run 2d version')
p <- add_argument(p, "--run3d", flag=TRUE, help = 'Run 3d version')
p <- add_argument(p, "--run4d", flag=TRUE, help = 'Run 4d version')
argv <- parse_args(p)

message("Reading in data...")
system.time(nii <- readRDS("results/nii.xyzt"))

# Save output
if (argv$run2d) {

    message("Reading in threshold...")
    system.time(thr <- readRDS('results/thresh.xy'))

    message("Thresholding 1 slice...")
    system.time(roi_mask <- threshold.img(nii[,,1,1], thr))
    saveRDS(roi_mask, "results/roi_mask.xy")
}

if (argv$run3d) {

    message("Reading in threshold...")
    system.time(thr <- readRDS('results/thresh.xyz'))

    message(sprintf("Thresholding a volume i.e. %d slices", dim(nii)[3]))
    system.time(roi_mask <- threshold.img(nii[,,,1], thr, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
    saveRDS(roi_mask, "results/roi_mask.xyz")
} 

if (argv$run4d) {

    message("Reading in threshold...")
    system.time(thr <- readRDS('results/thresh.xyzt'))

    message(sprintf("Find threshold of a 4D array i.e. %d slices", dim(nii)[3] * dim(nii)[4]))
    system.time(roi_mask <- threshold.img(nii, thr, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
    saveRDS(roi_mask, "results/roi_mask.xyzt")
}