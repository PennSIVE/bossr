
devtools::load_all()

message('Reading ROI mask...')
system.time(roi_mask <- readRDS('results/roi_mask_filtered.xyzt'))

message('Connecting components...')
system.time(roi_labels <- connect.components(roi_mask))
saveRDS(roi_labels, 'results/roi_labels.xyzt')