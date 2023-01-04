cd $(dirname $0)/../..

bsub -J 00_prepare_array -R "rusage[mem=100000]" -n 60 -o dev/logs/00_prepare_array.log -e dev/logs/00_prepare_array.log \
    Rscript dev/cli/00_prepare_array.R