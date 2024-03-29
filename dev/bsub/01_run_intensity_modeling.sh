cd $(dirname $0)/../..

bsub -J 01_intensity_modeling_2d -R "rusage[mem=500000]" -o dev/logs/01_intensity_modeling_2d.log -e dev/logs/01_intensity_modeling_2d.log \
    Rscript dev/cli/01_intensity_modeling.R --run2d

bsub -J 01_intensity_modeling_3d -R "rusage[mem=500000]" -n 201 -o dev/logs/01_intensity_modeling_3d.log -e dev/logs/01_intensity_modeling_3d.log \
    Rscript dev/cli/01_intensity_modeling.R --run3d

bsub -J 01_intensity_modeling_4d -R "rusage[mem=500000]" -n 201 -o dev/logs/01_intensity_modeling_3d.log -e dev/logs/01_intensity_modeling_3d.log \
    Rscript dev/cli/01_intensity_modeling.R --run4d