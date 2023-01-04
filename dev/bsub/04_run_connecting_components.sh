cd $(dirname $0)/../..

bsub -J 04_connecting_components -R "rusage[mem=100000]" -o dev/logs/04_connecting_components.log -e dev/logs/04_connecting_components.log \
    Rscript dev/cli/04_connecting_components.R