#!/bin/bash

# This script is called by bench_allsims.sh, so all the sbatch options are set by the sbatch call
# in bench_allsim.sh.
# This script runs S3N and SSN each once (one rep) for a single network and number of observations.
# Inputs: nw, nobs, nomp (value for OMP_NUM_THREADS)

source /home/r61g491/.bashrc
mamba activate r4.5.1.S3N

# name the three inputs to this script from bench_allsims.sh
nw=$1
nobs=$2
nomp=$3
# set rep and nreps based on the slurm array specs set by bench_allsims.sh
rep=$((SLURM_ARRAY_TASK_ID+1)) # each job ID corresponds to a rep number, 1 through nreps
nreps=$(($SLURM_ARRAY_TASK_COUNT)) # number of reps = number of array tasks
# report these values in the stdout file
echo "JobID ${SLURM_JOB_ID} (${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}), network ${nw}, nobs ${nobs}, rep ${rep} of ${nreps}"

# Update the job name to include the unique task job ID (%j or SLURM_JOB_ID), which is different from 
# the parent array ID, the task ID, and parentid_taskid
# scontrol update jobid=$SLURM_JOB_ID jobname="bench_${nw}_${nobs}_${rep}_${nreps}_${SLURM_JOB_ID}"

# set up multithreading (4 threads)
export OMP_NUM_THREADS=$nomp
echo "OMP_NUM_THREADS: ${OMP_NUM_THREADS}"

printf -v rep2d "%02d" $rep

echo "Checking if file bench_results/SSN_results_network${nw}_nobs${nobs}_rep${rep2d}.rda exists already..."

if [ ! -f bench_results/SSN_results_network${nw}_nobs${nobs}_rep${rep2d}.rda ]; then 

	# starttime
	date "+%Y-%m-%d %H:%M:%S"
	echo "Starting S3N benchmarking..."
	# call the R script to run S3N and SSN preprocessing and estimation for this rep of this simulation
	Rscript 2_bench_one_sim_S3N.R $nw $nobs $rep $nreps &> rout_S3N/bench_${nw}_${nobs}_${rep}_${nreps}.Rout
	
	date "+%Y-%m-%d %H:%M:%S"
	echo "Starting SSN benchmarking..."
	Rscript 2_bench_one_sim_SSN.R $nw $nobs $rep $nreps &> rout_SSN/bench_${nw}_${nobs}_${rep}_${nreps}.Rout
	# end time
	
	date "+%Y-%m-%d %H:%M:%S"
	echo "Done."

else echo "Already run."
fi

