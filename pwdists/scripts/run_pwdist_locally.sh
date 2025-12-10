#!/bin/bash

nprocs=4
num_batches=$1
batch_size=$2
nneighbors=$3

echo num_batches $num_batches
echo batch_size $batch_size
echo nneighbors $nneighbors

for SLURM_ARRAY_TASK_ID in `seq 1 $num_batches`; do

    export SLURM_ARRAY_TASK_ID
    njobs=`jobs | wc -l`

    while [ $njobs -ge $nprocs ]; do
      sleep 1
      njobs=`jobs | wc -l`
    done
    bash ./scripts/all_predsobs.sh "$@" &

    done
wait
