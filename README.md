# How to use this code

## Simulations

Since there are 950 simulations of both S3N and SSN, the code for the simulations is set up to use slurm. Clone this repo to your high-performance computing cluster.

To reproduce the simulations on a high-performance computing cluster using slurm:

1. Preprocess the initial streams data used for these simulations: `sbatch 0_bench_gen_streamsregion5.sh`
2. Generate the initial data files used for each simulation: `sbatch 1_bench_gen_initial_data.sh`
3. Run the simulations: `./bench_allsims.sh`
    - This generates a slurm array job with 50 replicates (tasks) for each parameter combination in `bench_params.txt`
4. Compile the results and generate plots and summary statistics: Either download the results to your computer and run `bench_and_validate.R` locally, or run this R script on the cluster

## Region 5

To reproduce the Region 5 results,

1. Run `Region5.R`, using `Region5cluster.R` at the appropriate time (see comments in `Region5.R`) to generate the pairwise distances on a cluster
    - Computing pairwise distances with S3N will be updated soon to work efficiently on a laptop.
2. Render `Region5_diagnostics.qmd` to obtain the additional diagnostics on the results.
