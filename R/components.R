# subset based on last dimension when dimensionality is not known
# last_ind(arr, n) is equivalent to arr[,,n] and arr[,,,n] for 
last_ind <- function(arr, n){
    nd <- length(dim(arr))
    # uncomment the following line if x could be a plain vector without dim
    # if(nd == 0) nd = 1
    inds <- rep(alist(,)[1], nd)
    inds[nd] <- n
    do.call(`[`, c(list(arr), inds))
}

get_adjacent <- function(arr, k){
    #' returns values that span at least k cells over `d`, i.e. last dim. of arr
    dlength <- rev(dim(arr))[1]
    # for each elem. in d 
    adj.vals <- purrr::map(1:dlength, ~unique(as.vector(last_ind(arr, .x)))) |> # list uniques at each t
        slider::slide(~purrr::reduce(.x, intersect), .after = k - 1) |> # take intersects in rolling windows of size `k - 1`
        purrr::reduce(c) |> # concatenate 
        unique() |> 
        sort()
    adj.vals[-1] # skip 0
}
get_size <- function(arr, label) data.frame(sum(arr == label))
get_centroid <- function(arr, label){
    centroid <- which(arr == label, arr.ind = TRUE) |>
        apply(2, mean) |> 
        as.list()
    centroid
}
make_cell_df <- function(arr, label) {
    if(label)
    list(label, get_size(arr, label), get_centroid(arr, label)) |> 
    purrr::reduce(c) |>
    as.data.frame() |>
    setNames(c('index','size', 'x', 'y', 'z')) #|>
    #dplyr::filter(size > size.thr)
}

#' @export
connect.components <- function(roi_mask, r = 21){
    
    k <- mmand::shapeKernel(c(r,r,3,3), type='box')
    roi_labels <- mmand::components(roi_mask, k)

    return(roi_labels)
}

#' @export
track.components <- function(roi_labels, k = 2, size.thr = 10){
    #' 1. subset array based on Z <- should this be done outside the function?
    #' 2. split into list based on t
    #' 3. take unique for each split (map)
    #' 4. slide over window of 2 and reduce(intersection) -> unique.value
    #' 5. save unique values 
    T <- dim(roi_labels)[4]
    Z <- dim(roi_labels)[3]

    # TODO: abstract pattern for z or t
    T.adj.vals <- get_adjacent(roi_labels, k)
    cell_df <- purrr::map(1:T, ~ get_adjacent(roi_labels[,,,.x], k)) |>
        purrr::map(~intersect(T.adj.vals)) |>
        purrr::imap(~ if(length(.x) != 0) make_cell_df(roi_labels[,,,.y], .x) |> cbind(t = .y)) |> 
        dplyr::bind_rows() |>
        dplyr::filter(size > size.thr)
    
    cell_df
}

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

#' @export
annot.dat <- function(dat, t){
    index <- sort(unique(dat$index))
  
    result.index <- c()
    for(i in index){
      index.X <- dat[which(dat$index==i),"X"]
      index.Y <- dat[which(dat$index==i),"Y"]
      index.Z <- dat[which(dat$index==i),"Z"]

      new.dat <- dat %>% filter(X <= max(index.X)+30 & X >= min(index.X)-30) %>% 
        filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) %>%
        filter(Z <= max(index.Z)+5 & Z >= min(index.Z)-5) 

      if(length(unique(new.dat$index)) >1 && length(unique(new.dat$T)) == nrow(new.dat)){
        print(new.dat)
        index.t <- c()
        for(j in unique(new.dat$index)){
          index.t <- c(index.t, dat[which(dat$index == j),"T"])
        }

        if(length(index.t) == length(unique(index.t))){
          result.index<- rbind(result.index, sort(unique(new.dat$index)))}
        else{
          print("Good")
        }

      }else{
        print("Good")
      }
    }
    
    result.index <- result.index[which(duplicated(result.index)==FALSE),]
    for(i in 1:nrow(result.index)){
      dat$index[which(dat$index==result.index[i,2])] <- result.index[i,1]
    }
    return(dat)
}