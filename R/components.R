#' Subset based on last dimension
#'
#' The function returns the values of an array at a specific index along the last dimension of the array
#'
#' @param arr array to subset
#' @param n index of last dimension to subset on
#' @return array subsetted on last dimension at index `n`
#' @noRd
last_ind <- function(arr, n){
    nd <- length(dim(arr))
    # uncomment the following line if x could be a plain vector without dim
    # if(nd == 0) nd = 1
    inds <- rep(alist(,)[1], nd)
    inds[nd] <- n
    do.call(`[`, c(list(arr), inds))
}

#' Gets k-long values
#'
#' The function returns values that span at least k cells over last dimension of the array
#'
#' @param arr array to subset
#' @param k minimum number of adjacent values
#' @return unique values that span at least k cells over last dimension of the array
#' @noRd
get_k_long_labels <- function(arr, k){

    dlength <- rev(dim(arr))[1]
    # for each elem. in d 
    adj.vals <- purrr::map(1:dlength, ~unique(as.vector(last_ind(arr, .x)))) |> # list uniques at each t (or z)
        slider::slide(~purrr::reduce(.x, intersect), .after = k - 1) |> # take intersects in rolling windows of size `k - 1`
        purrr::reduce(c) |> # concatenate 
        unique() |> 
        sort()
    adj.vals[-1] # skip 0
}

#' Get size of a value in an array
#'
#' The function returns the sum of elements in an array that match a specific value
#'
#' @param arr array
#' @param label value to get size of
#' @return sum of elements in array that match label
#' @noRd
get_size <- function(arr, label) data.frame(sum(arr == label, na.rm = TRUE))

#'Get centroid of a value in an array
#'
#' The function returns the centroid of elements in an array that match a specific value
#'
#' @param arr array
#' @param label value to get centroid of
#' @return centroid of elements in array that match label
#' @noRd
get_centroid <- function(arr, label){
    centroid <- which(arr == label, arr.ind = TRUE) |>
        apply(2, mean) |> 
        as.list()
    centroid
}

#' Create dataframe for a value in an array
#'
#' The function creates a dataframe from the values of size and centroid of elements in an array that match a specific value
#'
#' @param arr array
#' @param label value to make dataframe of
#' @return dataframe of size and centroid of elements in array that match label
#' @noRd
make_cell_df <- function(arr, label) {
    list(label, get_size(arr, label), get_centroid(arr, label)) |> 
    purrr::reduce(c) |>
    as.data.frame() |>
    stats::setNames(c('index','size', 'x', 'y', 'z')) 
}

#' Connected components in an array
#'
#' The function finds the connected components in an array and returns a labeled array of the same size
#'
#' @param roi_mask array to find connected components in
#' @param r radius of the connected component filter
#' @return labeled array of the same size
#' @export
connect_components <- function(roi_mask, r = 21){
    
    k <- mmand::shapeKernel(c(r,r,3,3), type='box')
    roi_labels <- mmand::components(roi_mask, k)

    return(roi_labels)
}

#' Track connected components in 4D array
#'
#' The function tracks the connected components in a 4D array and returns a dataframe of the tracked components
#'
#' @param roi_labels labeled array of connected components
#' @param k minimum number of adjacent frames for a component to be tracked
#' @param size.thr minimum size of component to be tracked
#' @return dataframe of tracked components
#' @export
track_components <- function(roi_labels, k = 2, size.thr = 10){
  # 1. subset array based on Z <- should this be done outside the function?
  # 2. split into list based on t
  # 3. take unique for each split (map)
  # 4. slide over window of 2 and reduce(intersection) -> unique.value
  # 5. save unique values 
  T <- dim(roi_labels)[4]
  Z <- dim(roi_labels)[3]
  
  # return labels with span >= k in T dimension
  T.adj.vals <- get_k_long_labels(roi_labels, k)
  cell_df <- purrr::map(1:T, ~ get_k_long_labels(roi_labels[,,,.x], k)) |> # at each T, get labels that span at least k-long in Z dimension
    # choose labels with sufficient span on both T and Z dimension. returns list where index are 1:T
    purrr::map(~intersect(.x, T.adj.vals)) |> 
    # for label (i) in timepoint (.y) get cell dimensions
    purrr::imap(~ if(length(.x) != 0) purrr::map(.x, function (i) make_cell_df(roi_labels[,,,.y], i) |> cbind(t = .y))) |> # .x is an intermediate vector that
    dplyr::bind_rows() |>
    dplyr::filter(size > size.thr)
  
  cell_df
}

#' Postprocceses a data.frame that is the result of track_components
#'
#' This function performs annotation on a data frame by sorting unique indexes and then filtering the dataframe based on X, Y, and Z coordinates. The function then checks for certain conditions and assigns new indexes to rows that meet those conditions.
#'
#' @param df dataframe
#' @return the annotated dataframe
#' @export
post_process_df <- function(df){
  
  # Create a vector index that consists of unique sorted values of the "index" column in df
  index <- sort(unique(df$index))
  
  # Initialize an empty vector result.index to store the result
  result.index <- c()
  
  # Loop over the values in index
  for(i in index){
    
    # Create vectors index.X, index.Y, index.Z, each consisting of the corresponding column values in df for the current value of i
    index.X <- df[which(df$index==i),"x"]
    index.Y <- df[which(df$index==i),"y"]
    index.Z <- df[which(df$index==i),"z"]
    
    # Filter df using dplyr to create new.df with rows that meet the criteria in the three filter statements
    new.df <- df |> dplyr::filter(x <= max(index.X)+30 & x>= min(index.X)-30) |> 
      dplyr::filter(y <= max(index.Y)+30 & y >= min(index.Y)-30) |>
      dplyr::filter(z <= max(index.Z)+5 & z >= min(index.Z)-5) 
    
    # Check if the length of unique values of the "index" column in new.df is greater than 1 and equal to the number of rows in new.df 
    if(length(unique(new.df$index)) >1 && length(unique(new.df$t)) == nrow(new.df)){
      
      # Initialize an empty vector index.t
      index.t <- c()
      
      # Loop over the unique values of the "index" column in new.df
      for(j in unique(new.df$index)){
        # Append the corresponding "t" column values in df to index.t for the current value of j
        index.t <- c(index.t, df[which(df$index == j),"t"])
      }
      
      # Check if the length of index.t is equal to the length of unique values of index.t
      if(length(index.t) == length(unique(index.t))){
        # If true, add the sorted unique values of the "index" column in new.df to result.index
        result.index<- rbind(result.index, sort(unique(new.df$index)))
      }
    }
  }
  
  # Remove duplicates from result.index
  result.index <- result.index[which(duplicated(result.index)==FALSE),]
  
  # Check if post-processing was needed
  if (!is.null(result.index)){
    # Replace the "index" column values in df with the corresponding values in result.index
    for(i in 1:nrow(result.index)){
      df$index[which(df$index==result.index[i,2])] <- result.index[i,1]
    }
  }
  
  for(i in df$index){
    
    cell.df <- df %>% filter(index ==i)
    
    if(length(unique(cell.df$t)) != (max(cell.df$t) - min(cell.df$t) +1)){
      sequence.T <- min(cell.df$t):max(cell.df$t)
      missing.T <- sequence.T[which(! sequence.T %in% cell.df$t)]
      
      impute.rows <- c()   
      for(tt in missing.T){ 
        impute.row <- cbind(i, mean(cell.df$size), mean(cell.df$x), mean(cell.df$y), mean(cell.df$z), tt)
        impute.rows <- rbind(impute.rows, impute.row)
      }
      
      colnames(impute.rows) <- colnames(df)
      df <- rbind(df, impute.rows)
      
    }
  }
  
  # Return df
  return(df)
}

