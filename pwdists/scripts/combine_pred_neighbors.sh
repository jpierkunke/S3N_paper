#!/bin/bash
#SBATCH --job-name combine_pred_neighbors
#SBATCH --partition short
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH --nodes 1
#SBATCH --output std_outfiles/combine_pred_neighbors_%j.out

num_batches=$1
batch_size=$2
nneighbors=$3

echo There are $num_batches batches of size $batch_size
echo with $nneighbors neighbors per pred point

if [ ! -z $SLURM_JOB_NAME ]; then module load R-bundle-CRAN; echo "module loaded"; else echo "module not loaded"; fi

Rscript code/combine_pred_neighbors.R $batch_size $nneighbors &> R_outfiles/combine_pred_neighbors_$batch_size_$nneighbors.Rout

