# this script combines the pred_neighbor variables from multiple batches 
# before prediction with BRISC
# Jess Kunke, Apr 2025

library(sf) # for reading and possibly writing shapefiles
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(labelled)

args <- commandArgs(trailingOnly = TRUE)
batch_size = as.numeric(args[1])
m = as.numeric(args[2])

# assumes this script is in a subfolder of the directory containing the other folders
base_dir = getwd()
cat("base_dir", fill=TRUE)
# location of obs neighbors for prediction (an input to this script)
obs_dir = paste0(base_dir, "/output_obsobs/")
# location to write combined preds neighbors (output from this script)
out_dir = paste0(base_dir, "/output_predsobs/")

source(paste0(base_dir, "/../S3N_functions.R"))

cat("Combine preds-obs nearest neighbor results", fill = TRUE)
combine_preds_nn_results(
  out_dir, # read in preds neighbor batches from this folder
  out_dir, # write combined preds neighbors to this same folder
  m,
  batch_size)

add_obs_nns_for_prediction(
  obs_dir, 
  out_dir)



