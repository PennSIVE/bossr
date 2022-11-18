library(argparser) |> suppressMessages()

# Set options and load package
devtools::load_all()
options(error = quote({dump.frames(to.file=TRUE); q(status=1)}))

p <- arg_parser("Run intensity modeling for function profiling")
p <- add_argument(p, "--run2d", flag=TRUE, help = 'Run 2d version')
p <- add_argument(p, "--run3d", flag=TRUE, help = 'Run 3d version')
p <- add_argument(p, "--run4d", flag=TRUE, help = 'Run 4d version')
argv <- parse_args(p)

message('Reading array...')
system.time(nii <- readRDS('results/nii.xyzt')) 

# Save output
if (argv$run2d) {
  message("Finding threshold of 1 slice...")
  system.time(thresh.xy <- betamix.2d(nii[,,1,1]))
  saveRDS(thresh.xy, "results/thresh.xy")
}

if (argv$run3d) {
  message(sprintf("Finding threshold of a volume i.e. %d slices", dim(nii)[3]))
  system.time(thresh.xyz <- betamix.3d(nii[,,,1], n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(thresh.xyz, "results/thresh.xyz")
} 

if (argv$run4d) {
  message(sprintf("Find threshold of a 4D array i.e. %d slices", dim(nii)[3] * dim(nii)[4]))
  system.time(thresh.xyzt <- betamix.4d(nii, n.cores=Sys.getenv('LSB_DJOB_NUMPROC')))
  saveRDS(thresh.xyzt, "results/thresh.xyzt")
}