#!/bin/bash

# For each parameter combination (stream network and number of observation points) in
# the input file bench_params.txt, this script submits a slurm array job of 50 replicates.
# Each replicate runs S3N and SSN once back to back (preprocessing and estimation for S3N, 
# followed by preprocessing and estimation for SSN).

# bench_params.txt should have four unnamed columns corresponding to these variables:
#   - nw = network number (1, 2, 3, 4, 5, or 6)
#   - nobs = number of observations (e.g. 100)
#   - time_limit = slurm job time limit in hh:mm:ss format
#   - mem_alloc = slurm job memory allocation request (e.g. 5G for 5 GB)
#   - time_limit and mem_alloc have been chosen based on previous run stats
# each line of this text file corresponds to a set of parameters and job specs
nparam_combos=$(wc -l < bench_params.txt) # total number of types of sims we will do
nomp=4 # value to which we will set OMP_NUM_THREADS for all sims

echo "Number of parameter combinations: ${nparam_combos}"

# for each line of bench_param_combos.txt (each parameter combination)...
param_combo=1
while read -r nw nobs time_limit mem_alloc; do
        
        # Ensure we don't process empty lines
        if [ -z "$nw" ]; then continue; fi

        echo "------------------------------------------------------------"
        echo "Network ${nw}, nobs ${nobs}, param combo ${param_combo} of ${nparam_combos}"
        echo "Time limit ${time_limit}, memory requested ${mem_alloc}"

        sbatch --account group-mathematicalsciences \
               --partition nextgen \
               --job-name "bench_${nw}_${nobs}" \
               --time "$time_limit" \
               --mem "$mem_alloc" \
               --cpus-per-task "$nomp" \
               --output stdout/bench_${nw}_${nobs}_%a.out \
               --error stdout/bench_${nw}_${nobs}_%a.out \
               --array 0-49 \
               bench_one_sim.sh $nw $nobs $nomp $SLURM_JOB_ID
        
        ((param_combo++))

done < bench_params.txt

echo "Done submitting array jobs for benchmarking."
