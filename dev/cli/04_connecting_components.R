
devtools::load_all()

message('Reading ROI mask...')
system.time(roi_mask <- readRDS('dev/results/roi_mask_filtered.xyzt'))

message('Connecting components...')
system.time(roi_labels <- connect.components(roi_mask))
saveRDS(roi_labels, 'dev/results/roi_labels.xyzt')