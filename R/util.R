list2nifti <- function(list, dims) list |> unlist() |> array(dims) |> oro.nifti::as.nifti() 
