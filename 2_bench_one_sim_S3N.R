# this script runs a single sim (rep) each for S3N and SSN for a given 
# stream network (nw) and number of observation points (nobs)

library(sf) # for reading and possibly writing shapefiles
library(knitr) # for making tables
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(magrittr) # for the eager pipe
library(latex2exp) # for LaTeX formatting in plot labels
library(SSNbler) # to benchmark SSN preprocessing
library(SSN2) # to benchmark SSN distances and estimation
library(S3N) # S3N estimation and prediction

tic("Start of script")

# inputs needed to script:
# bench_and_validate_rep.R $nw $nobs $rep $nreps
args <- commandArgs(trailingOnly = TRUE)
nw = as.numeric(args[1])
nobs = as.numeric(args[2])
rep = as.numeric(args[3])
nreps = as.numeric(args[4])
nreps_S3N = 1
nreps_SSN = 1

# set filepaths
base_dir = paste0(getwd(), "/")
cat(paste0("Base dir: ", base_dir))
data_path = "input_data/"
covs_path = paste0(data_path, "envir_covs/") # environmental covariate data
streams_path = paste0(data_path, "flowlines/")  # flowline layer for Region 5
pred_path = paste0(data_path, "predpoints/") # prediction point layer for Region 5
comid_huc12_path = paste0(data_path, "match_COMID_to_HUC12/") # data matching COMIDs to HUC12s

# default location for where to save preprocessing output from this script
preproc_output = "pwdists/input/" # make subfolder with job id?
# where to save data to use as input in computing pairwise distances
pwdist_input_dir = preproc_output
# where to find the preds-obs pairwise distance results from the cluster
pwdist_predsobs_dir = "pwdists/output_predsobs/" # make subfolder with job id?
# where to find the obs-obs pairwise distance results from the cluster
pwdist_obsobs_dir = "pwdists/output_obsobs/" # make subfolder with job id?
bench_res_dir = "bench_results/"

source(paste0(base_dir, "S3N_functions.R")) # make sure this points to the right functions

# set lsn file path and output path for benchmark/validation results
ndigits = identify_ndigits(nreps)
lsn_rep = str_pad(rep, ndigits, side = "left", pad = "0")
lsn_path = paste0("lsn/lsn_nw", nw, "_nobs", nobs, "_rep", lsn_rep)

toc() # start of script

message(paste(
  "Starting S3N benchmarking/validation for network", nw, 
  "with", nobs, "observations for rep", rep, "of", nreps
))

tic("S3N code overall")
# run S3N code to simulate responses for estimation (S3N is faster for
# computing the pwdists necessary to simulate responses)
if(!is.na(nreps_S3N)){
  benchmark_S3N_one_rep(
    nw, nobs, rep, nreps,
    out_dir = bench_res_dir
  )
}

cat("", fill = TRUE)
toc() # S3N overall
cat("", fill = TRUE)

message(paste0(
  "S3N simulation is done for network ", nw, 
  " with ", nobs, " obs for rep ", rep, " of ", nreps, "."
))

