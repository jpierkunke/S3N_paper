# Region 5 fish population estimates with S3N
# PhD dissertation, Chapter 4
# Jess Kunke, 2025 April

library(sf) # for reading and possibly writing shapefiles
library(plyr) # for rbind.fill
library(knitr) # for making tables
library(beepr) # for notifying when a task is done
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(mapview) # for visualizing spatial data
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(magrittr) # for the eager pipe
library(SSNbler) # to benchmark SSN preprocessing
library(SSN2) # to benchmark SSN distances and estimation
library(S3N) # S3N estimation and prediction

source("S3N_functions.R")

# set file paths
data_path = "input_data/"
covs_path = paste0(data_path, "envir_covs/") # environmental covariate data
streams_path = paste0(data_path, "flowlines/")  # flowline layer for Region 5
pred_path = paste0(data_path, "predpoints/") # prediction point layer for Region 5
comid_huc12_path = paste0(data_path, "match_COMID_to_HUC12/") # data matching COMIDs to HUC12s
fish_path = paste0(data_path, "fish/") # fish survey data

# default location for where to save preprocessing output from this script
preproc_output = "pwdists/input/"
# where to save data to use as input in computing pairwise distances
pwdist_input_dir = "pwdists/input/"
# where to find the preds-obs pairwise distance results from the cluster
pwdist_predsobs_dir = "pwdists/output_predsobs/"
# where to find the obs-obs pairwise distance results from the cluster
pwdist_obsobs_dir = "pwdists/output_obsobs/"

load(paste0(pwdist_input_dir, "preds_obs_pwdist_input_data.RData"))

# Compute obs-obs pairwise distances and neighbor variables ---------------

# obs-obs distances are used in both estimation and prediction
# neighbor variables are just for estimation

tic("Obs-obs distances")
system('cd pwdists; ./scripts/all_obsobs.sh')
toc() # Obs-obs distances: sec elapsed

# Compute preds-obs pairwise distances and neighbor variables for prediction --

tic("Preds-obs distances")
# determine batch number and size based on number of (unobserved) prediction points
batch_info = get_preds_batch_info( nrow(preds) - nrow(obs) )
m=10 # use 10 neighbors
args = sprintf("%i %i %i", batch_info$n, batch_info$size, m)
system(paste('cd pwdists; ./scripts/run_pwdist_locally.sh', args))
system(paste('cd pwdists; ./scripts/combine_pred_neighbors.sh', args))
toc() # Preds-obs distances: sec elapsed

