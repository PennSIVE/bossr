cd $(dirname $0)/../..

bsub -J 01_intensity_modeling_2d -q taki_normal -R "rusage[mem=500000]" -o logs/01_intensity_modeling_2d.log -e logs/01_intensity_modeling_2d.log \
    Rscript dev/cli/01_intensity_modeling.R --run2d

bsub -J 01_intensity_modeling_3d -q taki_normal -R "rusage[mem=500000]" -n 201 -o logs/01_intensity_modeling_3d.log -e logs/01_intensity_modeling_3d.log \
    Rscript dev/cli/01_intensity_modeling.R --run3d

bsub -J 01_intensity_modeling_4d -q taki_normal -R "rusage[mem=500000]" -n 201 -o logs/01_intensity_modeling_3d.log -e logs/01_intensity_modeling_3d.log \
    Rscript dev/cli/01_intensity_modeling.R --run4d