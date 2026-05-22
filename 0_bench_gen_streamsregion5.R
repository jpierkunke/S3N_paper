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

# assumes working directory is S3N_paper
source("S3N_functions.R")

# set filepaths
data_path = "input_data/"
cat(paste0("Data path: ", data_path))
covs_path = paste0(data_path, "envir_covs/") # environmental covariate data
streams_path = paste0(data_path, "flowlines/")  # flowline layer for Region 5
pred_path = paste0(data_path, "predpoints/") # prediction point layer for Region 5
comid_huc12_path = paste0(data_path, "match_COMID_to_HUC12/") # data matching COMIDs to HUC12s

# default location for where to save preprocessing output from this script
preproc_output = "pwdists/input/"
# where to save data to use as input in computing pairwise distances
pwdist_input_dir = "pwdists/input/"
# where to find the preds-obs pairwise distance results from the cluster
pwdist_predsobs_dir = "pwdists/output_predsobs/"
# where to find the obs-obs pairwise distance results from the cluster
pwdist_obsobs_dir = "pwdists/output_obsobs/"
bench_res_dir = "bench_results/"

## first, clean the streams data to get the largest network, Region 5 ----------

# download stream flowlines and pred points layer for region of interest from here:
# https://research.fs.usda.gov/rmrs/projects/national-stream-internet#download-data
# and set streams_path and pred_path below to the locations of these files

# download all the environmental covariate data, put it in a folder together,
# and set covs_path below to the location of that folder

# download fish datasets, put them in a folder together, and set fish_path below
# to that location

# download the file "HUC12_PU_COMIDs_CONUS.csv" and put it in the location
# designated below by comid_huc12_path

### envir covars preprocessing ----------------------------------------------

# only need to do this once unless the preprocessing changes; uncomment if needed

# # combine environmental covariates and write to csv file
# # note default input: comid_huc12_filename = "HUC12_PU_COMIDs_CONUS.csv"
# combine_and_write_national_covariate_data(covs_path, comid_huc12_path)

### streams data preprocessing ----------------------------------------------

# read in streams flowlines for a given region,
# join the combined envir covariate data,
# reassign duplicate COMIDs as negative COMIDs,
# impute and rename covariates,
# write imputed renamed covariates to file
cat("Starting streams data preprocessing", fill = TRUE)

tic("streams data preprocessing")
streams = read_sf(streams_path, "Flowline_MS05_NSI") %!>% # 169463 obs of 16 vars
  # must join envir covar before reassigning dup COMIDs or else the dup COMIDs won't get HUC codes or envir covars
  join_envir_covar_data(covs_path) %!>%
  reassign_dup_COMIDs() %!>%
  # dplyr::select(COMID, LENGTHKM, TotDASqKM, geometry) %!>%
  impute_and_rename_covars()
toc() # 4.478 sec on new laptop, 14.35 sec elapsed (14-15 sec) on old laptop
# 169463 obs of 27 vars (without the select line above)

### stream network preprocessing ----------------------------------------------

#### build the stream network ---------------
cat("Starting to build the stream network", fill = TRUE)
tic("configure")
streams_res = configure_stream_network(streams)
toc() # 14 sec on new laptop, 56.729 sec elapsed (55-65 sec) on old laptop
streams = streams_res$streams
stream_graphs = streams_res$sg
# now streams has 169463 obs of 36 vars
# outflow:
#     0      1 
#    63 169400
# inflow:
#     0     1     2     3 
# 64095 41337 64030     1
# This network has 63 separate components.

#### explore the complex confluence ---------------
# complex_conf = names(which(table(streams$dnstream_node) == 3))
# # "1151540.2996 2059592.6663"
# streams$complex = (streams$dnstream_node == complex_conf)
# mapview(filter(streams, HUC8 == "05040002"), zcol = c("complex", "COMID"))

#### explore the source node "downstream divergences" ---------------
down_div = names(which(table(streams$upstream_node) > 1)) # 14 downstream divergences

cat("Removing source node downstream divergences", fill = TRUE)
upnd_div = streams$COMID[streams$upstream_node %in% down_div] # 33 of these

#### remove smallest branch of complex confluence ---------------
# - COMID 167484513 and all reaches upstream of it
cat("Removing complex confluence", fill = TRUE)
complex_conf = 167484513
upnd1 = streams$upstream_node[streams$COMID == complex_conf]
dnnd1 = streams$COMID[streams$dnstream_node == upnd1] # 15411283
upnd2 = streams$upstream_node[streams$COMID == dnnd1]
dnnd2 = streams$COMID[streams$dnstream_node == upnd2] # 15409971 15410029
upnd3 = streams$upstream_node[streams$COMID %in% dnnd2]
dnnd3 = streams$COMID[streams$dnstream_node %in% upnd3] # 15409975 15409973
upnd4 = streams$upstream_node[streams$COMID %in% dnnd3]
dnnd4 = streams$COMID[streams$dnstream_node %in% upnd4] # 15409981 15411355
upnd5 = streams$upstream_node[streams$COMID %in% dnnd4]
dnnd5 = streams$COMID[streams$dnstream_node %in% upnd5] # 15411353 15411309
upnd6 = streams$upstream_node[streams$COMID %in% dnnd5]
dnnd6 = streams$COMID[streams$dnstream_node %in% upnd6] # empty

COMIDs_to_drop = c(complex_conf, 
                   upnd_div,
                   dnnd1, 
                   dnnd2, 
                   dnnd3, 
                   dnnd4, 
                   dnnd5, 
                   dnnd6)

# the rest of this code is the same as before, except that I rename it from 
# nocomplex to dropcomplex

# remove these 10 COMIDs and also any COMIDs outside of the main component
streams2 = streams %>%
  filter(!(COMID %in% COMIDs_to_drop), componentID == 1)
# now streams has 169094 obs (369 fewer COMIDs) or 169091?, still 36 vars
100*nrow(streams2)/nrow(streams) # we are still keeping 99.76% of the network
streams = streams2; rm(streams2)

#### build the stream network again ---------------
cat("Rebuilding the stream network", fill = TRUE)
tic()
streams_res = configure_stream_network(streams)
toc() # 14.3 sec on new laptop, 63.004 sec elapsed (55-65 sec) on my old laptop
streams = streams_res$streams
stream_graphs = streams_res$sg
rm(streams_res)
# still 169091 obs of 36 vars
# outflow:
#     0      1 
#     1 169060 
# inflow: used to be
#     0      1      2 
# 63905  41285  63904 
# now
#     0      1      2 
# 63886  41290  63885 
# This network has 1 separate component.

# verify no downstream divergences or complex confluences
names(which(table(streams$dnstream_node) > 2)) # complex confluences
names(which(table(streams$upstream_node) > 1)) # downstream divergences

# save region 5 up to this point
save(streams, stream_graphs, file = paste0(bench_res_dir, "streams_region5_allvars.rda"))

load(paste0(bench_res_dir, "streams_region5_allvars.rda"))
streams = select(streams, COMID, HUC8, HUC12, LENGTHKM, TotDASqKM, 
                 Development, Agriculture, Elevation, Water_Area, 
                 Ann_Runoff, Baseflow, Ann_Temp, Hydro_Alter, 
                 Fldplain_Dis, geometry)
streams$HUC10 = str_sub(streams$HUC12, end = 10)
streams$HUC6 = str_sub(streams$HUC12, end = 6)
streams$HUC4 = str_sub(streams$HUC12, end = 4)
streams$HUC2 = str_sub(streams$HUC12, end = 2)
save(streams, file = paste0(bench_res_dir, "streams_region5.rda"))
rm(stream_graphs)
cat("Done building and saving the streams object.", fill = TRUE)

# building the LSN again, SSN finds a new topological error:
# lsn_path = "lsn"
# # start_time = Sys.time()
# edges <- lines_to_lsn(
#   streams = streams,
#   lsn_path = lsn_path,
#   check_topology = TRUE,
#   snap_tolerance = 0, #0.05,
#   topo_tolerance = 0, #20,
#   overwrite = TRUE,
#   use_parallel = TRUE,
#   no_cores = 4
# )
# st_coordinates(topo_err)
#            X       Y
# [1,] 1367441 2083863
streams_bu = streams
load(paste0(bench_res_dir, "streams_region5_allvars.rda"))
# View(streams |> filter(up_X < 1367442 & up_X > 1367440))
streams_problem = streams |> filter(up_X < 1367442 & up_X > 1367440)
# st_distance(
#   lwgeom::st_startpoint(streams_problem$geometry[1]),
#   lwgeom::st_startpoint(streams_problem$geometry[2])
# )
# # Units: [m]
# # [,1]
# # [1,] 0.05013791
# st_distance(
#   lwgeom::st_startpoint(streams_problem$geometry[1]),
#   lwgeom::st_endpoint(streams_problem$geometry[1])
# )
# # Units: [m]
# # [,1]
# # [1,] 0.05013791
# st_distance(
#   lwgeom::st_startpoint(streams_problem$geometry[1]),
#   lwgeom::st_endpoint(streams_problem$geometry[2])
# )
# # Units: [m]
# # [,1]
# # [1,] 138.9406
# sum(streams$LENGTHKM < 0.01) # number of reaches shorter than 10 m
# [1] 614
# sum(streams$LENGTHKM < 0.005) # number of reaches shorter than 5 m
# [1] 202

# it thinks these two reaches share an upstream node, but actually one flows 
# into the other
# resolve this by merge the geometries and omit the shorter reach
streams$geometry[streams$COMID == 4742285] = st_line_merge(st_combine(streams_problem$geometry[1:2]))
streams = streams |> filter(COMID != 4742287)

cat("Rebuilding the stream network", fill = TRUE)
tic()
streams_res = configure_stream_network(streams)
toc() # 63.004 sec elapsed (55-65 sec)
streams = streams_res$streams
stream_graphs = streams_res$sg
rm(streams_res)

streams = select(streams, COMID, HUC8, HUC12, LENGTHKM, TotDASqKM, 
                 Development, Agriculture, Elevation, Water_Area, 
                 Ann_Runoff, Baseflow, Ann_Temp, Hydro_Alter, 
                 Fldplain_Dis, geometry)
streams$HUC10 = str_sub(streams$HUC12, end = 10)
streams$HUC6 = str_sub(streams$HUC12, end = 6)
streams$HUC4 = str_sub(streams$HUC12, end = 4)
streams$HUC2 = str_sub(streams$HUC12, end = 2)
save(streams, file = paste0(bench_res_dir, "streams_region5.rda"))
rm(stream_graphs)
cat("Done building and saving the streams object.", fill = TRUE)

# lsn_path = "lsn"; library(beepr)
# edges <- lines_to_lsn(
#   streams = streams,
#   lsn_path = lsn_path,
#   check_topology = TRUE,
#   snap_tolerance = 0, #0.05,
#   topo_tolerance = 0, #20,
#   overwrite = TRUE,
#   use_parallel = TRUE,
#   no_cores = 4
# ); beep(3)




