library(tiff)
library(oro.nifti)

# read in image
tiff.in <- tiff::readTIFF("data/C2-30.tif", all=TRUE, info=TRUE)

# function to extract tiff info
extract_dimensions <- function(tiff){ 
    tiff.info <- attr(tiff[[1]], "description")
    lengths.str <- unlist(strsplit(tiff.info, split = "\n"))[3] 
    dims <- as.numeric(stringr::str_extract_all(lengths.str, '[0-9]+')[[1]])
    names(dims) <- c("x", "y", "z", "t")
    return(dims)
}

# Resolution information will be used later
extract_resolutions <- function(tiff){
    resolutions.str <- c(attr(tiff[[1]], "x.resolution"), 
                    attr(tiff[[1]], "y.resolution"))
    resolutions <- as.numeric(resolutions.str)
    names(resolutions) <- c("x", "y")
    return(resolutions)
}

# Save list to array
list2nifti <- function(list, dims) list |> unlist() |> array(dims) |> oro.nifti::as.nifti() 

# Rotate 90 clockwise along xy-plane
rotate.xy <- function(mat) mat |> apply(2, rev) |> t()

# Given a timepoint t rotates a volume
rotate.xyz <- function(arr, n.cores=1, ...){
        dims <- dim(arr)
        1:dims[3] |> 
            parallel::mclapply(function(z) rotate.xy(arr[,,z]), mc.cores=n.cores, ...) |> 
            list2nifti(dims)
}

rotate.xyzt <- function(arr, n.cores=1, ...){
    dims <- dim(arr)
    Z <- dims[3]
    T <- dims[4]
    inputs <- expand.grid(1:Z, 1:T) # split by rows
    # iterate over rows of inputs to treat
    rotate_from_inputs <- function(i){
            z <- inputs[i, 1]
            t <- inputs[i, 2]
            rotate.xy(arr[,,z,t])
    }
    1:nrow(inputs) |> 
        parallel::mclapply(rotate_from_inputs,
        mc.cores=n.cores, ...) |> 
        list2nifti(dims)
}
# then rotate

dims <- extract_dimensions(tiff.in)
resl  <- extract_resolutions(tiff.in)

message("Converting list to nifti")
system.time(nii <- list2nifti(tiff.in, dims)) # TODO: time it 

# Save outputs
message("Rotating a slice")
system.time(nii.xy <- rotate.xy(nii[,,1,1]))
saveRDS(nii.xy, "dev/results/nii.xy")

message(sprintf("Rotating a volume i.e. %d", dim(nii)[3]))
system.time(nii.xyz <- rotate.xyz(nii[,,,1]))
saveRDS(nii.xyz, "dev/results/nii.xyz")

message(sprintf("Rotating 4D array i.e. %d slices", dim(nii)[3] * dim(nii)[4]))
system.time(nii.xyzt <- rotate.xyzt(nii))
saveRDS(nii.xyzt, "dev/results/nii.xyzt")
