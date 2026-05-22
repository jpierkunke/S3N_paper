#!/bin/bash
#SBATCH --account=priority-jesskunke
#SBATCH --job-name=bench_gen_streamsregion5
#SBATCH --partition=priority
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --time=1:00:00
#SBATCH --output=std_outfiles/genReg5_%j.out
#SBATCH --error=std_outfiles/genReg5_%j.out

source /home/r61g491/.bashrc
mamba activate r4.5.1.S3N

Rscript bench_gen_streamsregion5.R &> R_outfiles/genReg5.Rout

