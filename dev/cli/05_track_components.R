devtools::load_all()

roi_labels <- readRDS('results/roi_labels.xyzt')

label_df <- track.components(roi_labels)
delta_n <- get.delta.n(label_df)

write.csv(label_df, 'results/num_df.csv')
saveRDS(delta_n, 'results/summ.rds')