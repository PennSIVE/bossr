library(betareg) |> suppressMessages()
library(scales) |> suppressMessages()
library(dplyr) |> suppressMessages()
library(parallel) |> suppressMessages()
library(argparser) |> suppressMessages()

p <- arg_parser("Run intensity modeling for function profiling")
p <- add_argument(p, "--run2d", flag=TRUE, help = 'Run 2d version')
p <- add_argument(p, "--run3d", flag=TRUE, help = 'Run 3d version')
p <- add_argument(p, "--run4d", flag=TRUE, help = 'Run 4d version')
argv <- parse_args(p)

list2nifti <- function(list, dims) list |> unlist() |> array(dims) |> oro.nifti::as.nifti() 

betamix.2d <- function(arr, mixnum = 2){

  x <- as.vector(arr)
  min.intensity <- quantile(x, probs = 0.99) |> unname()
  if (max(x) == 0){
    warning("No signal in slice: All values 0. Returning `NA`")
    return(NA)
  }
  x <- x [x > min.intensity]
  original.scale.x <- range(x)
  
  threshold <- quantile(x, 0.8) 
  x <- scales::rescale(x, to = c(0.0001,0.9999), from = original.scale.x)
  
  x.beta <- data.frame(y = x)
  m <- betamix(y ~ 1, data = x.beta, k = 1:mixnum)
  
  if (length(unique(clusters(m))) > 1){ 
    
    mu <- plogis(coef(m)[,1])
    phi <- exp(coef(m)[,2])
    
    a <- mu * phi
    b <- (1 - mu) * phi
    cl <- clusters(m)
    
    ys <- seq(0, 1, by = 0.01)
    
    if ( mu[1] > mu[2] ){
        density.difference <- dbeta(ys, shape1 = a[1], shape2 = b[1]) - 
                              dbeta(ys, shape1 = a[2], shape2 = b[2])
    } else {
        density.difference <- dbeta(ys, shape1 = a[2], shape2 = b[2]) - 
                              dbeta(ys, shape1 = a[1], shape2 = b[1])
    }
    
    intersection.point <- (which(diff(density.difference > 0) != 0) + 1)
    
    if (any(intersection.point>=100)){
      intersection.point <- intersection.point[-which(intersection.point>=100)]
    
      for (i in 1:length(intersection.point)){
        if(density.difference[intersection.point[i]+1] - density.difference[intersection.point[i]-1] > 0){
          before.threshold <- ys[intersection.point[i]]
        }
      }
      threshold <- scales::rescale(before.threshold, to = original.scale.x, from = c(0.0001,0.9999))
    } 
  }
  return(threshold)
}

betamix.3d <- function(arr, mixnum=2, n.cores=1, ...){
  #' Takes a 3D array
  dims <- dim(arr)
  Z <- dims[3]
  threshold <- parallel::mclapply(1:Z, function(z) betamix.2d(arr[,,z], mixnum), mc.cores=n.cores, ...) |> 
    list2nifti(Z)
  return(threshold)
}

betamix.4d <- function(arr, mixnum=2, n.cores=1, ...) {
  dims <- dim(arr)
  Z <- dims[3]
  T <- dims[4]
  inputs <- expand.grid(1:Z, 1:T) # split by rows
  
  betamix_from_inputs <- function(i){
            z <- inputs[i, 1]
            t <- inputs[i, 2]
            
            betamix.2d(arr[,,z,t], mixnum=mixnum)
  }
  threshold <- parallel::mclapply(1:nrow(inputs), 
                                  betamix_from_inputs,
                                  mc.cores = n.cores, 
                                  ...)
  threshold <- list2nifti(threshold, c(Z,T))
  return(threshold)
}

betamix.img <- function(arr, mixnum=2, n.cores=1, ...){
    
    ndim.img <- length(dim(arr))

    if (ndim.img == 2){ # img is a slice/matrix, thr should be one number
      betamix.2d(arr, mixnum=mixnum)

    } else if (ndim.img == 3){ # img is volume, thr should be vector
      betamix.3d(arr, mixnum=mixnum, n.cores=n.cores, ...)

    } else if (ndim.img == 4){ # img is 4D array, thr should be a matrix
      betamix(arr, mixnum=mixnum, n.cores=n.cores, ...)
  }
}


options(error = quote({dump.frames(to.file=TRUE); q(status=1)}))
message('Reading array...')
system.time(nii <- readRDS('results/nii.xyzt')) 

# Save output
if (argv$run2d) {
  message("Finding threshold of 1 slice...")
  system.time(thresh.xy <- betamix.2d(nii[,,1,1]))
  saveRDS(thresh.xy, "results/thresh.xy")
}

if (argv$run3d) {
  message(sprintf("Finding threshold of a volume i.e. %d slices", dim(nii)[3]))
  system.time(thresh.xyz <- betamix.3d(nii[,,,1], n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(thresh.xyz, "results/thresh.xyz")
} 

if (argv$run4d) {
  message(sprintf("Find threshold of a 4D array i.e. %d slices", dim(nii)[3] * dim(nii)[4]))
  system.time(thresh.xyzt <- betamix.4d(nii, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(thresh.xyzt, "results/thresh.xyzt")
}