cd $(dirname $0)/../..

bsub -J 05_track_components -R "rusage[mem=100000]" -o dev/logs/05_track_components.log -e dev/logs/05_track_components.log \
    Rscript dev/cli/05_track_components.R