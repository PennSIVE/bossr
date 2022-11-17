cd $(dirname $0)/../..

bsub -J 02_thresholding_2d -q taki_normal -R "rusage[mem=500000]" -o logs/02_thresholding_2d.log -e logs/02_thresholding_2d.log \
    Rscript dev/cli/02_thresholding.R --run2d

bsub -J 02_thresholding_3d -q taki_normal -R "rusage[mem=500000]" -n 201 -o logs/02_thresholding_3d.log -e logs/02_thresholding_3d.log \
    Rscript dev/cli/02_thresholding.R --run3d

bsub -J 02_thresholding_4d -q taki_normal -R "rusage[mem=500000]" -n 201 -o logs/02_thresholding_4d.log -e logs/02_thresholding_4d.log \
    Rscript dev/cli/02_thresholding.R --run4d