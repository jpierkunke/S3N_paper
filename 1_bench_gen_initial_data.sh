#!/bin/bash
#SBATCH --account=priority-jesskunke
#SBATCH --job-name=bench_gen_initial_data
#SBATCH --partition=priority
#SBATCH --cpus-per-task=1
#SBATCH --mem=3G
#SBATCH --time=1:00:00
#SBATCH --output=std_outfiles/benchinput_%j.out
#SBATCH --error=std_outfiles/benchinput_%j.out
#SBATCH --array=0-18

# this script runs multiple parameter combinations (network and number of observation points) for many reps
# each rep is a different array task that runs a background job for each parameter combination
# this way each task has a mix of slow parameter combinations and fast parameter combinations, so each task should take approximately
# the same amount of time.
# in other words, each task does 1 rep of every parameter combination, and each task is a replicate of each other task.

source /home/r61g491/.bashrc
mamba activate r4.5.1.S3N

param_combo=$((SLURM_ARRAY_TASK_ID+1)) # each job ID corresponds to a line of the param combos text file, i.e. a param combination

echo "task id ${param_combo}"

# read in text file with all the parameter combinations
# read -r nw nobs <<< '$(sed -n "{$param_combo}{p;q}" bench_param_combos.txt)'
nw=$(sed -n "$param_combo{p;q}" bench_param_combos.txt | cut -d" " -f1)
nobs=$(sed -n "$param_combo{p;q}" bench_param_combos.txt | cut -d" " -f2)

echo "Generating input data for network ${nw}, nobs ${nobs}"

Rscript 1_bench_gen_initial_data.R $nw $nobs &> R_outfiles/benchinput_${nw}_${nobs}.Rout

echo "Done."
