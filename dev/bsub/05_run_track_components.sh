cd $(dirname $0)/../..

bsub -J 05_track_components -R "rusage[mem=100000]" -o logs/05_track_components.log -e logs/05_track_components.log \
    Rscript dev/cli/05_track_components.R