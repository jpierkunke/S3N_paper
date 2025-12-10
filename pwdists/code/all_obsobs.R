# this script computes obs-obs distances and neighbor variables for 
# use in BRISC estimation and in computing pred-obs distances for BRISC prediction
# Jess Kunke, Apr 2025

library(sf) # for reading and possibly writing shapefiles
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(labelled)

args <- commandArgs(trailingOnly = TRUE)
m = as.numeric(args[1])

# assumes this script is in a subfolder of the directory containing the other folders
base_dir = getwd()
print(base_dir)
out_dir = paste0(base_dir, "/output_obsobs/")
data_dir = paste0(base_dir, "/input/")
print(data_dir)

source(paste0(base_dir, "/../S3N_functions.R"))

load(paste0(data_dir, "preds_obs_pwdist_input_data.RData"))

compute_pwdists_pred_obs(obs, obs, streams, m, out_dir, obs_only = TRUE)





