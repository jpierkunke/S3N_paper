# script to compute pairwise distances between preds and obs points and
# identify nearest obs neighbors to each pred point
# Jess Kunke, Apr 2025

library(sf) # for reading and possibly writing shapefiles
library(igraph) # for making the adjacency matrix
library(tictoc) # for benchmarking (timing to evaluate efficiency/runtime)
library(Matrix) # for efficient sparse adjacency matrix operations
library(tidyverse) # for efficient data manipulation
library(shapefiles) # for read.dbf
library(labelled)

args <- commandArgs(trailingOnly = TRUE)
batch_id = as.numeric(args[1])
batch_size = as.numeric(args[2])
m = as.numeric(args[3])

cat(paste("Batch ID", batch_id, "with batch size", batch_size, 
          "and", m, "neighbors per preds point"), fill=TRUE)

# assumes this script is in a subfolder of the directory containing the other folders
base_dir = getwd()
cat(base_dir, fill=TRUE)
out_dir = paste0(base_dir, "/output_predsobs/")
data_dir = paste0(base_dir, "/input/")

source(paste0(base_dir, "/../S3N_functions.R"))

# first compute the pairwise distances --------------------------------------#

# should contain an sf object called obs that has one row per COMID where
# fish were observed and COMID is in preds
load(paste0(data_dir,"preds_obs_pwdist_input_data.RData"))

if(n_distinct(obs$COMID) != nrow(obs)){
  cat(paste("obs contains", n_distinct(obs$COMID), "unique COMIDs, but obs has", nrow(obs), "rows."), fill=TRUE)
  stop("obs should have exactly one row per COMID.")
}

if(m > nrow(obs)){
  stop("Number of nearest neighbors m is larger than the number of observation points nrow(obs).")
}

out_file_suffix = paste0(str_pad(batch_id, 2, pad ="0"), "_", 
                         m, "nn_batchsize_", batch_size, ".rda")

n = nrow(obs)
preds_only = preds[-(1:n),] # excludes obs locations

start_ind = batch_size*(batch_id-1) + 1
end_ind = min(batch_size*batch_id, nrow(preds_only))

if(start_ind > end_ind){
  message("This batch is unnecessary; previous batches have already computed all pred-obs variables. Exiting now.")
  stop_quietly()
}
if(start_ind <=0){
  stop("start_ind is not positive.")
}

cat(paste("Filename suffix for output files:", out_file_suffix), fill=TRUE)

cat(paste("Starting batch", batch_id), fill=TRUE)

cat(paste("Processing prediction locations", start_ind + n, "to", end_ind + n, "..."), fill=TRUE)
start_job = Sys.time()
compute_pwdists_pred_obs(preds_only[start_ind:end_ind,], obs, streams, m,
                         out_dir, out_file_suffix = out_file_suffix)

end_job = Sys.time()
runtime = end_job - start_job
cat(paste("Runtime:", runtime), fill = TRUE)

