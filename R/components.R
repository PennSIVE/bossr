#' @export
connect.components <- function(roi_mask, r = 21){
    
    k <- mmand::shapeKernel(c(r,r,3,3), type='box')
    roi_labels <- mmand::components(roi_mask, k)

    return(roi_labels)
}
