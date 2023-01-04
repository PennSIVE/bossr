library(argparser) |> suppressMessages()

devtools::load_all()

# Run 
p <- arg_parser("Run intensity modeling for function profiling")
p <- add_argument(p, "--run2d", flag=TRUE, help = 'Run 2d version')
p <- add_argument(p, "--run3d", flag=TRUE, help = 'Run 3d version')
p <- add_argument(p, "--run4d", flag=TRUE, help = 'Run 4d version')
argv <- parse_args(p)

message("Reading in data...")
system.time(nii <- readRDS("dev/results/nii.xyzt"))

# Save output
if (argv$run2d) {

    message("Reading in threshold...")
    system.time(thr <- readRDS('dev/results/thresh.xy'))

    message("Thresholding 1 slice...")
    system.time(roi_mask <- threshold.img(nii[,,1,1], thr))
    saveRDS(roi_mask, "dev/results/roi_mask.xy")
}

if (argv$run3d) {

    message("Reading in threshold...")
    system.time(thr <- readRDS('dev/results/thresh.xyz'))

    message(sprintf("Thresholding a volume i.e. %d slices", dim(nii)[3]))
    system.time(roi_mask <- threshold.img(nii[,,,1], thr, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
    saveRDS(roi_mask, "dev/results/roi_mask.xyz")
} 

if (argv$run4d) {

    message("Reading in threshold...")
    system.time(thr <- readRDS('dev/results/thresh.xyzt'))

    message(sprintf("Find threshold of a 4D array i.e. %d slices", dim(nii)[3] * dim(nii)[4]))
    system.time(roi_mask <- threshold.img(nii, thr, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
    saveRDS(roi_mask, "dev/results/roi_mask.xyzt")
}