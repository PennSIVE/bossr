library(mmand)
library(oro.nifti)

connect.components <- function(roi_mask, r = 21){
    
    k <- mmand::shapeKernel(c(r,r,3,3), type='box')
    roi_labels <- mmand::components(roi_mask, k)

    # TODO: if slices == NULL select a default scope
    return(roi_labels)
}

message('Reading ROI mask...')
system.time(roi_mask <- readRDS('results/roi_mask_filtered.xyzt'))

message('Connecting components...')
system.time(roi_labels <- connect.components(roi_mask))
saveRDS(roi_labels, 'results/roi_labels.xyzt')