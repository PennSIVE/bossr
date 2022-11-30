cd $(dirname $0)/../..

bsub -J 04_connecting_components -R "rusage[mem=100000]" -o logs/04_connecting_components.log -e logs/04_connecting_components.log \
    Rscript dev/cli/04_connecting_components.R