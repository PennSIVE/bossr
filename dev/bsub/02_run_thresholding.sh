cd $(dirname $0)/../..

bsub -J 02_thresholding_2d -R "rusage[mem=500000]" -o dev/logs/02_thresholding_2d.log -e dev/logs/02_thresholding_2d.log \
    Rscript dev/cli/02_thresholding.R --run2d

bsub -J 02_thresholding_3d -R "rusage[mem=500000]" -n 201 -o dev/logs/02_thresholding_3d.log -e dev/logs/02_thresholding_3d.log \
    Rscript dev/cli/02_thresholding.R --run3d

bsub -J 02_thresholding_4d -R "rusage[mem=500000]" -n 201 -o dev/logs/02_thresholding_4d.log -e dev/logs/02_thresholding_4d.log \
    Rscript dev/cli/02_thresholding.R --run4d