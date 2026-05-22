# generate the initial data files for all parameter combinations
# before the slurm job array with all the replicates is submitted,
# because each array task will have just one replicate for each param combo,
# so each array task has to access the same data file already created

library(sf) # for reading and possibly writing shapefiles
# library(knitr) # for making tables
# library(igraph) # for making the adjacency matrix
# library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(magrittr) # for the eager pipe
# library(latex2exp) # for LaTeX formatting in plot labels
# library(SSNbler) # to benchmark SSN preprocessing
# library(SSN2) # to benchmark SSN distances and estimation
# library(S3N) # S3N estimation and prediction

# inputs needed to script:
# bench_gen_initial_data.R $nw $nobs
args <- commandArgs(trailingOnly = TRUE)
nw = as.numeric(args[1])
nobs = as.numeric(args[2])

message(paste0("Generating input data for network ", nw, " and nobs ", nobs, "..."))

# set filepaths
base_dir = paste0(getwd(), "/")
message(paste0("Base dir: ", base_dir))
data_path = "input_data/"
pred_path = paste0(data_path, "predpoints/") # prediction point layer for Region 5
bench_res_dir = "bench_results/" # make subfolder with job id? I think not.

outfn = paste0(bench_res_dir, "network", nw, "_nobs", nobs, "_initial_data.rda")

source(paste0(base_dir, "S3N_functions.R")) # make sure this points to the right functions

# load Network 6 streams network
load(paste0(bench_res_dir, "streams_region5.rda"))

if(nw == 1){
  streams = filter(streams, HUC10 == "0514020607")
}

if(nw == 2){
  streams = filter(streams, HUC8 == "05140206")
}

if(nw == 3){
  streams = filter(streams, HUC6 == "051402")
}

if(nw == 4){
  streams = filter(streams, HUC4 == "0514")
}

if(nw == 5){
  streams = filter(streams, HUC4 %in% c("0514", "0513"))
}

if(nw == 6){
#  streams = filter(streams, HUC2 == "05")
}

# keep only inputs needed for S3N and/or SSN
streams = select(streams, COMID, LENGTHKM, TotDASqKM, Elevation)
preds = generate_benchmark_preds(streams, pred_path)
obs = generate_benchmark_obs(preds, nobs)

save(streams, preds, obs, file = outfn)

message(paste0("Data written to ", outfn, "."))

