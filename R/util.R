#' Convert a list of arrays to a Nifti object
#'
#' list2nifti() converts a list of arrays to a Nifti object using the oro.nifti package.
#'
#' @param list A list of arrays that will be converted to a Nifti object.
#' @param dims A vector of dimensions for the resulting array.
#' @returns A Nifti object created from the input list of arrays.
#' @examples
#' list2nifti(list(array(1:12, c(3, 4)), array(13:24, c(3, 4))), c(3, 4, 2))
#'
#' # convert a list of 3D arrays to a 4D Nifti object
#' list2nifti(list(array(1:12, c(3, 4, 3)), array(13:24, c(3, 4, 3))), c(3, 4, 3, 2))
#' @noRd
list2nifti <- function(list, dims) list |> unlist() |> array(dims) |> oro.nifti::as.nifti() 
