#' Subset based on last dimension
#'
#' The function returns the values of an array at a specific index along the last dimension of the array
#'
#' @param arr array to subset
#' @param n index of last dimension to subset on
#' @return array subsetted on last dimension at index `n`
#' @examples
#' last_ind(arr, 5)
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
#' @examples
#' get_k_long_labels(arr, 2)
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
#' @examples
#' get_size(arr, 10)
get_size <- function(arr, label) data.frame(sum(arr == label, na.rm = TRUE))

#'Get centroid of a value in an array
#'
#' The function returns the centroid of elements in an array that match a specific value
#'
#' @param arr array
#' @param label value to get centroid of
#' @return centroid of elements in array that match label
#' @examples
#' get_centroid(arr, 10)
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
#' @examples
#' make_cell_df(arr, 10)
make_cell_df <- function(arr, label) {
    list(label, get_size(arr, label), get_centroid(arr, label)) |> 
    purrr::reduce(c) |>
    as.data.frame() |>
    setNames(c('index','size', 'x', 'y', 'z')) 
}

#' Connected components in an array
#'
#' The function finds the connected components in an array and returns a labeled array of the same size
#'
#' @param roi_mask array to find connected components in
#' @param r radius of the connected component filter
#' @return labeled array of the same size
#' @examples
#' connect.components(arr, 10)
#' @export
connect.components <- function(roi_mask, r = 21){
    
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
#' @examples
#' track.components(arr, 2, 10)
#' @export
track.components <- function(roi_labels, k = 2, size.thr = 10){
  # 1. subset array based on Z <- should this be done outside the function?
  # 2. split into list based on t
  # 3. take unique for each split (map)
  # 4. slide over window of 2 and reduce(intersection) -> unique.value
  # 5. save unique values 
  T <- dim(roi_labels)[4]
  Z <- dim(roi_labels)[3]
  
  # return labels with span >= k in T dimension
  T.adj.vals <- get_k_long(roi_labels, k)
  cell_df <- purrr::map(1:T, ~ get_k_long(roi_labels[,,,.x], k)) |> # at each T, get labels that span at least k-long in Z dimension
    # choose labels with sufficient span on both T and Z dimension. returns list where index are 1:T
    purrr::map(~intersect(.x, T.adj.vals)) |> 
    # for label (i) in timepoint (.y) get cell dimensions
    purrr::imap(~ if(length(.x) != 0) purrr::map(.x, function (i) make_cell_df(roi_labels[,,,.y], i) |> cbind(t = .y))) |> # .x is an intermediate vector that
    dplyr::bind_rows() |>
    dplyr::filter(size > size.thr)
  
  cell_df
}

#' Get number of connected components in each timepoint of 4D array
#'
#' The function returns the number of connected components in each timepoint of a 4D array
#'
#' @param cell_df dataframe of tracked components
#' @return number of connected components in each timepoint
#' @examples
#' get.delta.n(cell_df)
#' @export
get.delta.n <- function(cell_df){

    # get number of cells in each timepoint
    ncells <- cell_df |> 
        split(cell_df$t) |>
        purrr::map_int(~nrow(.x))

    ## assign 0s for timepoints without cells
    Ts <- names(ncells) |> as.numeric()
    max_t <- max(Ts)
    diff <- setdiff(1:max_t, Ts)
    ncells <- c(ncells, rep(0, length(diff))) |> setNames(c(names(ncells), diff))

    # sort by t
    ncells <- ncells[order(as.numeric(names(ncells)))]
    added <- slider::slide_int(ncells, ~.x[2] - .x[1], .before=1)
    added
}

#' Annotates data
#'
#' This function performs annotation on a data frame by sorting unique indexes and then filtering the dataframe based on X, Y, and Z coordinates. The function then checks for certain conditions and assigns new indexes to rows that meet those conditions.
#'
#' @param df dataframe
#' @param t 
#' @return the annotated dataframe
#' @export
annotate.df <- function(df, t){
    index <- sort(unique(df$index))
  
    result.index <- c()
    for(i in index){
      index.X <- df[which(df$index==i),"x"]
      index.Y <- df[which(df$index==i),"y"]
      index.Z <- df[which(df$index==i),"z"]

      new.df <- df |> filter(x <= max(index.X)+30 & x>= min(index.X)-30) |> 
        filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) |>
        filter(Z <= max(index.Z)+5 & Z >= min(index.Z)-5) 

      if(length(unique(new.df$index)) >1 && length(unique(new.df$T)) == nrow(new.df)){
        print(new.df)
        index.t <- c()
        for(j in unique(new.df$index)){
          index.t <- c(index.t, df[which(df$index == j),"t"])
        }

        if(length(index.t) == length(unique(index.t))){
          result.index<- rbind(result.index, sort(unique(new.df$index)))}
        else{
          print("Good")
        }

      }else{
        print("Good")
      }
    }

    result.index <- result.index[which(duplicated(result.index)==FALSE),]
    for(i in 1:nrow(result.index)){
      df$index[which(df$index==result.index[i,2])] <- result.index[i,1]
    }
    return(df)
}
