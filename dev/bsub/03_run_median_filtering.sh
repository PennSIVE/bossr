cd $(dirname $0)/../..

bsub -J 03_median_filtering_3d -R "rusage[mem=500000]" -o dev/logs/03_median_filtering_3d.log -e dev/logs/03_median_filtering_3d.log \
    Rscript dev/cli/03_median_filtering.R --run3d

bsub -J 03_median_filtering_4d -R "rusage[mem=500000]" -n 12 -o dev/logs/03_median_filtering_4d.log -e dev/logs/03_median_filtering_4d.log \
    Rscript dev/cli/03_median_filtering.R --run4d