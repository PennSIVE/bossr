devtools::load_all()

roi_labels <- readRDS('dev/results/roi_labels.xyzt')

label_df <- track.components(roi_labels)
delta_n <- get.delta.n(label_df)

write.csv(label_df, 'dev/results/num_df.csv')
saveRDS(delta_n, 'dev/results/summ.rds')