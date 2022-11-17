cd $(dirname $0)/../..

bsub -J 00_prepare_array -q taki_normal -R "rusage[mem=100000]" -n 60 -o logs/00_prepare_array.log -e logs/00_prepare_array.log \
    Rscript dev/cli/00_prepare_array.R