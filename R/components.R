
#' @export
connect.components <- function(roi_mask, r = 21){
    
    k <- mmand::shapeKernel(c(r,r,3,3), type='box')
    roi_labels <- mmand::components(roi_mask, k)

    return(roi_labels)
}

subset_arr <- function(){
    narray::subset(arr, list(1:2, 1:2, 1:2, 1:3))
}

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

#' @export
track.components <- function(roi_labels, z = NULL, k = 2, size.thr = 10){
    #' 1. subset array based on Z <- should this be done outside the function?
    #' 2. split into list based on t
    #' 3. take unique for each split (map)
    #' 4. slide over window of 2 and reduce(intersection) -> unique.value
    #' 5. save unique values 
    T <- dim(roi_labels)[4]
    Z <- dim(roi_labels)[3]

    # TODO: abstract pattern for z or t
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
    T.adj.vals <- purrr::map(1:T, ~unique(as.vector(last_ind(roi_labels, .x)))) |> # list uniques at each t
        slider::slide(~purrr::reduce(.x, intersect), .after = k - 1) |> # take intersects in rolling windows of size `k - 1`
        reduce(c) |> # concatenate 
        unique()[-1] # skip 0s
    
    get_size <- function(label) data.frame( sum(roi_labels[,,,t] == label))
    get_centroid <- function(label){
        centroid <- which(roi_labels[,,,t] == label, arr.ind = TRUE) |>
            apply(index.xyz, 2, mean) |> 
            as.list()
        centroid
    }

    # TODO: use map -> use future_map
    get_z_adjacent <- function(roi_labels){
        Z.adj.vals <- purrr::map(1:Z, ~unique(as.vector(roi_labels[,,.x]))) |> # list uniques at each z in current t
            slider::slide(~purrr::reduce(.x, intersect), .after = k - 1) |> # take intersects in rolling windows of size `k - 1`
            reduce(c) |> # concatenate 
            unique()[-1] # exclude 0
        #Z.adj.vals <- c(Z.adj.vals, Z.adj.val) # something wrong here?

        Z.adj.vals <- unique(Z.adj.vals) # something wrong here?
        Z.adj.vals
    }
    #Z.adj.vals <- c()
    purrr:map(1:T, ~ get_z_adjacent(roi_labels[,,,.x]) |> )
    for (t in 1:T){
        # 
        purrr:map(1:Z, get_z_adjacent(roi_labels)
        adj.vals <- intersect(Z.adj.vals, T.adj.vals)
        adj.vals
        Z.adj.vals <- purrr::map(1:Z, ~unique(as.vector(roi_labels[,,.x,t]))) |> # list uniques at each z in current t
            slider::slide(~purrr::reduce(.x, intersect), .after = k - 1) |> # take intersects in rolling windows of size `k - 1`
            reduce(c) |> # concatenate 
            unique()[-1] # exclude 0
        #Z.adj.vals <- c(Z.adj.vals, Z.adj.val) # something wrong here?

        Z.adj.vals <- unique(Z.adj.vals) # something wrong here?
        adj.vals <- intersect(Z.adj.vals, T.adj.vals)

        cell_df <- purrr::map(adj.vals, ~ list(.x, get_size(.x), get_centroid(.x), t)) |> 
            reduce(c) |>
            as.data.frame() |>
            setNames(c('index','size', 'x', 'y', 'z', 't')) |>
            dplyr::filter(size > size.thr)

        cell_df
    }
    # After mapping above dplyr::bind_rows()

}

#' TODO: regarrange function above
#' _ definitions out of loop
#' _ make the for loop a mapping
#' _ abstract pattern of contiguity check
#' _ should these be separate functions?
# Keep this?
get.z.adj <- function(roi_labels, z = NULL){
    if (is.null(z)) z = 1:dim(roi_labels)[3]


}