library(argparser)

devtools::load_all()
options(error = quote({dump.frames(to.file=TRUE); q(status=1)}))

# Run 
p <- arg_parser("Run intensity modeling for function profiling")
p <- add_argument(p, "--run3d", flag=TRUE, help = 'Run 3d version')
p <- add_argument(p, "--run4d", flag=TRUE, help = 'Run 4d version')
argv <- parse_args(p)

# Run
if (argv$run3d) {

  message('Reading ROI mask...')
  system.time(roi_mask <- readRDS('results/roi_mask.xyz')) 

  message("Applying median filter to a volume")
  system.time(roi_mask_filtered <- median.filtering(roi_mask, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(roi_mask_filtered, "results/roi_mask_filtered.xyz")
} 

if (argv$run4d) {

  message('Reading ROI mask...')
  system.time(roi_mask <- readRDS('results/roi_mask.xyzt'))

  message(sprintf("Applying median filter to 4D array i.e. %d volumes", dim(roi_mask)[4]))
  system.time(roi_mask_filtered <- median.filtering(roi_mask, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(roi_mask_filtered, "results/roi_mask_filtered.xyzt")
}