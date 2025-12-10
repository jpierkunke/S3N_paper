#!/bin/bash
#SBATCH --job-name all_predsobs
#SBATCH --partition short
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH --nodes 1
#SBATCH --output std_outfiles/all_predsobs_%2a.out
#SBATCH --array 1-34

batch_id=$SLURM_ARRAY_TASK_ID
batch_id_padded=`printf "%02d" $batch_id`
num_batches=$1
batch_size=$2
nneighbors=$3

echo batch_id $batch_id of $num_batches batches
echo batch_size $batch_size
echo $nneighbors neighbors per pred point

if [ ! -z $SLURM_JOB_NAME ]; then module load R-bundle-CRAN; echo "module loaded"; else echo "module not loaded"; fi

Rscript code/all_predsobs.R $batch_id $batch_size $nneighbors &> R_outfiles/all_predsobs_$batch_id_padded.Rout

echo batch_id $batch_id finished
