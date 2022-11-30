cd $(dirname $0)/../..

bsub -J 03_median_filtering_3d -R "rusage[mem=500000]" -o logs/03_median_filtering_3d.log -e logs/03_median_filtering_3d.log \
    Rscript dev/cli/03_median_filtering.R --run3d

bsub -J 03_median_filtering_4d -R "rusage[mem=500000]" -n 12 -o logs/03_median_filtering_4d.log -e logs/03_median_filtering_4d.log \
    Rscript dev/cli/03_median_filtering.R --run4d