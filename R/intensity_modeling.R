#' Calculate threshold from 2D array using beta mixture model
#'
#' This function takes a 2D array and the number of mixture components as inputs, and returns a threshold value. It first extracts values above a certain intensity from the array, scales these values between 0.0001 and 0.9999, fits a beta mixture model to these scaled values, and calculates a density difference between the two beta distributions. Finally, it scales the intersection point of these distributions back to the original scale of the array values and returns it as the threshold.
#'
#' @param arr A 2D array of values
#' @param mixnum The number of mixture components to use in the beta mixture model. Default is 2.
#' @return A threshold value
betamix_2d <- function(arr, mixnum = 2){

  x <- as.vector(arr)
  if (max(x) == 0){
    warning("No signal in slice: All values 0. Returning `NA`")
    return(NA)
  }
  q = 0.99
  min.intensity <- quantile(x, probs = q) 
  while(! any(x > min.intensity)){
    q = q - 0.01
    min.intensity <- quantile(x, probs = q)
  }
  x <- x [x > min.intensity]
  original.scale.x <- range(x)
  
  threshold <- quantile(x, 0.8) 
  x <- scales::rescale(x, to = c(0.0001,0.9999), from = original.scale.x)
  
  x.beta <- data.frame(y = x)
  m <- betareg::betamix(y ~ 1, data = x.beta, k = 1:mixnum)
  
  if (length(unique(modeltools::clusters(m))) > 1){ 
    
    mu <- plogis(coef(m)[,1])
    phi <- exp(coef(m)[,2])
    
    a <- mu * phi
    b <- (1 - mu) * phi
    
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

#' Calculate threshold from 3D array using beta mixture model
#'
#' This function takes a 3D array, the number of mixture components, number of cores and additional parameters as inputs, and returns a threshold value. It applies `betamix_2d` function on each slice of 3D array and  returns the threshold value of each slice.
#' 
#' @param arr A 3D array of values
#' @param mixnum The number of mixture components to use in the beta mixture model. Default is 2.
#' @param n.cores number of cores used in parallel computing. Default is 1.
#' @param ... additional parameters passed to `mclapply`
#' @return A threshold value
betamix_3d <- function(arr, mixnum=2, n.cores=1, ...){
  
  dims <- dim(arr)
  Z <- dims[3]
  threshold <- parallel::mclapply(1:Z, function(z) betamix_2d(arr[,,z], mixnum), mc.cores=n.cores, ...) |> 
    list2nifti(Z)
  return(threshold)
}

#' Calculate threshold from 4D array using beta mixture model
#'
#' This function takes a 4D array, the number of mixture components, number of cores and additional parameters as inputs, and returns a threshold value. It applies `betamix_3d` function on each time point of 4D array and  returns the threshold value of each time point.
#' 
#' @param arr A 4D array of values
#' @param mixnum The number of mixture components to use in the beta mixture model. Default is 2.
#' @param n.cores number of cores used in parallel computing. Default is 1.
#' @param ... additional parameters passed to `mclapply`
#' @return A threshold value
betamix_4d <- function(arr, mixnum=2, n.cores=1, ...) {
  dims <- dim(arr)
  Z <- dims[3]
  T <- dims[4]
  inputs <- expand.grid(1:Z, 1:T) # split by rows
  
  betamix_from_inputs <- function(i){
            z <- inputs[i, 1]
            t <- inputs[i, 2]
            
            betamix_2d(arr[,,z,t], mixnum=mixnum)
  }
  threshold <- parallel::mclapply(1:nrow(inputs), 
                                  betamix_from_inputs,
                                  mc.cores = n.cores, 
                                  ...)
  threshold <- list2nifti(threshold, c(Z,T))
  return(threshold)
}

#' Calculate threshold from image using beta mixture model
#'
#' This function takes an image array, number of mixture components, number of cores and additional parameters as inputs, and returns a threshold value. It detects the dimension of the image array and applies `betamix_2d()`, `betamix_3d()`, or `betamix_4d()` accordingly to calculate the threshold.
#'
#' @param arr image array
#' @param mixnum The number of mixture components to use in the beta mixture model. Default is 2.
#' @param n.cores number of cores used in parallel computing. Default is 1.
#' @param ... additional parameters passed to `betamix_3d()` and `betamix_4d()` functions
#' @return A threshold value
#' @export
betamix_img <- function(arr, mixnum=2, n.cores=1, ...){
    
    ndim.img <- length(dim(arr))

    if (ndim.img == 2){ # img is a slice/matrix, thr should be one number
      betamix_2d(arr, mixnum=mixnum)

    } else if (ndim.img == 3){ # img is volume, thr should be vector
      betamix_3d(arr, mixnum=mixnum, n.cores=n.cores, ...)

    } else if (ndim.img == 4){ # img is 4D array, thr should be a matrix
      betamix_4d(arr, mixnum=mixnum, n.cores=n.cores, ...)
  }
}
