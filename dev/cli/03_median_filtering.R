library(argparser)
library(mmand)

## Functions
list2nifti <- function(list, dims) list |> unlist() |> array(dims) |> oro.nifti::as.nifti() 

median.filtering.3d <- function(roi_mask, r = 10){
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

options(error = quote({dump.frames(to.file=TRUE); q(status=1)}))

# Run 
p <- arg_parser("Run intensity modeling for function profiling")
p <- add_argument(p, "--run3d", flag=TRUE, help = 'Run 3d version')
p <- add_argument(p, "--run4d", flag=TRUE, help = 'Run 4d version')
argv <- parse_args(p)

# Run
if (argv$run3d) {

  message('Reading ROI mask...')
  system.time(roi_mask <- readRDS('results/roi_mask.xyz')) 

  message("Applying median filter to a volume")
  system.time(roi_mask_filtered <- median.filtering(roi_mask, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(roi_mask_filtered, "results/roi_mask_filtered.xyz")
} 

if (argv$run4d) {

  message('Reading ROI mask...')
  system.time(roi_mask <- readRDS('results/roi_mask.xyzt'))

  message(sprintf("Applying median filter to 4D array i.e. %d volumes", dim(roi_mask)[4]))
  system.time(roi_mask_filtered <- median.filtering(roi_mask, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(roi_mask_filtered, "results/roi_mask_filtered.xyzt")
}