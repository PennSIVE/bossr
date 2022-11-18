
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

betamix.3d <- function(arr, mixnum=2, n.cores=1, ...){
  
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
