#!/bin/bash
#SBATCH --job-name all_obsobs
#SBATCH --partition short
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH --nodes 1
#SBATCH --output std_outfiles/all_obsobs_%j.out

nneighbors=10

if [ ! -z $SLURM_JOB_NAME ]; then module load R-bundle-CRAN; echo "module loaded"; else echo "module not loaded"; fi

Rscript code/all_obsobs.R $nneighbors &> R_outfiles/all_obsobs_$nneighbors.Rout


