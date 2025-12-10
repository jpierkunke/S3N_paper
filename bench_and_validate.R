# This script reproduces the benchmarking and validation results from the paper,
# including the figures

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
library(latex2exp) # for LaTeX formatting in plot labels
library(lemon) # for repositioning plot legend in empty facets
library(paletteer) # for colors in benchmark network maps
library(PNWColors) # for colors in benchmark network maps
library(SSNbler) # to benchmark SSN preprocessing
library(SSN2) # to benchmark SSN distances and estimation
library(BRISC) # version of BRISC I modified to handle S3N estimation and prediction
library(ggpattern) # for validation plots
library(fishualize) # for fish-inspired color palettes
library(patchwork) # to combine multiple graphs in a single figure

# assumes working directory is S3N_paper
source("S3N_functions.R")

# set filepaths
data_path = "input_data/"
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
tic("streams data preprocessing")
streams = read_sf(streams_path, "Flowline_MS05_NSI") %!>% # 169463 obs of 16 vars
  # must join envir covar before reassigning dup COMIDs or else the dup COMIDs won't get HUC codes or envir covars
  join_envir_covar_data(covs_path) %!>%
  reassign_dup_COMIDs() %!>%
  # dplyr::select(COMID, LENGTHKM, TotDASqKM, geometry) %!>%
  impute_and_rename_covars()
toc() # 14.35 sec elapsed (14-15 sec)
# 169463 obs of 27 vars (without the select line above)

### stream network preprocessing ----------------------------------------------

#### build the stream network ---------------
tic("configure")
streams_res = configure_stream_network(streams)
toc() # 56.729 sec elapsed (55-65 sec)
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

#### remove smallest branch of complex confluence ---------------
# - COMID 167484513 and all reaches upstream of it
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
# now streams has 169094 obs (369 fewer COMIDs), still 36 vars
100*nrow(streams2)/nrow(streams) # we are still keeping 99.78% of the network
streams = streams2; rm(streams2)

#### build the stream network again ---------------
tic()
streams_res = configure_stream_network(streams)
toc() # 63.004 sec elapsed (55-65 sec)
streams = streams_res$streams
stream_graphs = streams_res$sg
rm(streams_res)
# still 169094 obs of 36 vars
# outflow:
#     0      1 
#     1 169093 
# inflow:
#     0      1      2 
# 63905  41285  63904 
# This network has 1 separate component.

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

## benchmarking and validation ---------------------------

# load Network 6 streams network
load(paste0(bench_res_dir, "streams_region5.rda"))
# create a copy
streams_full = streams
# set lsn file path and output path for benchmark/validation results
lsn.path = "lsn"

# set up network parameters
bench_nws = data.frame(
  Network = 1:6,
  nreach = NA, # these will be filled in after the benchmarking and validation runs are complete
  npreds = NA, # these will be filled in after the benchmarking and validation runs are complete
  nobs = NA,   # these will be filled in after the benchmarking and validation runs are complete
  nreps_S3N = c(2, 2, 2, 2, 2, 2),   # for a quick test run
  nreps_SSN = c(2, 2, 2, 2,  2, NA), # for a quick test run
  # nreps_S3N = c(50, 50, 50, 10, 10, 10), # to reproduce the results from the paper
  # nreps_SSN = c(50, 50, 50, 10,  2, NA), # to reproduce the results from the paper
  preproc_only = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
)


### Subnetwork 1: HUC10 containing stream outlet (284 reaches) -------------

streams = filter(streams_full, HUC10 == "0514020607")
benchmark_and_validate(streams, pred_path,
                       bench_nws$Network[1],
                       bench_nws$nreps_S3N[1],
                       bench_nws$nreps_SSN[1],
                       bench_nws$preproc_only[1])


### Subnetwork 2: HUC8 containing stream outlet (1273 reaches) -------------

streams = filter(streams_full, HUC8 == "05140206")
benchmark_and_validate(streams, pred_path,
                       bench_nws$Network[2], 
                       bench_nws$nreps_S3N[2], 
                       bench_nws$nreps_SSN[2],
                       bench_nws$preproc_only[2])


### Subnetwork 3: HUC6 containing stream outlet (7146 reaches) -------------

streams = filter(streams_full, HUC6 == "051402")
benchmark_and_validate(streams, pred_path,
                       bench_nws$Network[3], 
                       bench_nws$nreps_S3N[3], 
                       bench_nws$nreps_SSN[3],
                       bench_nws$preproc_only[3])

### Subnetwork 4: HUC4 containing stream outlet (11540 reaches)-------------

streams = filter(streams_full, HUC4 == "0514")
benchmark_and_validate(streams, pred_path,
                       bench_nws$Network[4], 
                       bench_nws$nreps_S3N[4], 
                       bench_nws$nreps_SSN[4],
                       bench_nws$preproc_only[4])
# S3N: R session takes <=2 GB memory throughout, 
#      obs-obs distances takes 4-5 GB memory
#      preds-obs distances takes 1-2 GB memory per batch, 4 batches (1440 pred points each)
# SSN: R session takes 8-9 GB memory for building LSN, stream updist;
#      6-8 GB for AFV, ~1.3 GB to add obs to LSN, 4-6 GB for estimation


### Subnetwork 5: Two neighboring HUC4s (30748 reaches) --------------------

streams = filter(streams_full, HUC4 %in% c("0514", "0513"))
# benchmark_and_validate(streams, pred_path,
#                        bench_nws$Network[5], 
#                        bench_nws$nreps_S3N[5], 
#                        bench_nws$nreps_SSN[5])
# error in SSN in building LSN when nobs is capped at 1000:
# Error: vector memory limit of 16.0 Gb reached, see mem.maxVSize()
# 32 GB was also not enough
# so set it to 60 GB with mem.maxVSize(60000) and rerun
# indeed, top (in terminal) indicates max memory is > 48 GB
mem.maxVSize(60000)
benchmark_and_validate(streams, pred_path,
                       bench_nws$Network[5], 
                       bench_nws$nreps_S3N[5], 
                       bench_nws$nreps_SSN[5],
                       bench_nws$preproc_only[5])


### Subnetwork 6: HUC2 (169092 reaches) ------------------------------------

streams = filter(streams_full, HUC2 == "05")
benchmark_and_validate(streams, pred_path,
                       bench_nws$Network[6],
                       bench_nws$nreps_S3N[6], 
                       bench_nws$nreps_SSN[6],
                       bench_nws$preproc_only[6])
beep(3)





## Combine and summarize benchmarking results -----------------------------

# # loads obs, preds, streams, params, runtimes
# load("bench_results/S3N_results_network1_rep1.rda")
# runtimes_all = runtimes
# load("bench_results/S3N_results_network1_rep2.rda")
# runtimes_all = rbind(runtimes_all, runtimes) # (# reps) x 5
# 
# # S3N has 3 tasks if preproc_only = TRUE, 5 otherwise:
# # “Build LSN” = configure_stream_network
# # “Stream updist and AFV" = compute_stream_updist_vars
# # “Add obs to LSN” = prep_to_compute_pwdist
# # if not preproc_only,
# # “Obs-obs distances” = system('cd pwdists; ./scripts/all_obsobs.sh')
# # “Estimation” = BRISC_estimation_stream
# 
# load("bench_results/SSN_results_network1_rep1.rda")
# runtimes_all_SSN = runtimes
# load("bench_results/SSN_results_network1_rep2.rda")
# runtimes_all_SSN = rbind(runtimes_all_SSN, runtimes) # (# reps) x 5
# 
# # SSN has 4 tasks if preproc_only = TRUE, 6 otherwise:
# # “Build LSN” = lines_to_lsn
# # “Stream updist and AFV" = updist_edges and afv_edges
# # “Add obs to LSN” = sites_to_lsn and updist_sites and afv_sites
# # "Assemble SSN" = ssn_assemble
# # if not preproc_only,
# # “Obs-obs distances” = ssn_create_distmat
# # “Estimation” = ssn_lm
# 
# runtimes_S3N = combine_runtimes_onemodel(bench_res_dir, bench_nws$Network[1], bench_nws$nreps_S3N[1], 
#                                          "S3N", FALSE)
# runtimes_SSN = combine_runtimes_onemodel(bench_res_dir, bench_nws$Network[1], bench_nws$nreps_SSN[1], 
#                                          "SSN", FALSE)

for(nw in 1:6){
  load(paste0(bench_res_dir, "network", nw, "_initial_data.rda"))
  bench_nws$nreach[nw] = nrow(streams)
  bench_nws$npreds[nw] = nrow(preds)
  bench_nws$nobs[nw] = nrow(obs)
}

bench1 = combine_runtimes_bothmodels(
  bench_res_dir,
  bench_nws$Network[1],
  bench_nws$nreps_S3N[1],
  bench_nws$nreps_SSN[1],
  bench_nws$preproc_only[1])

bench2 = combine_runtimes_bothmodels(
  bench_res_dir,
  bench_nws$Network[2],
  bench_nws$nreps_S3N[2],
  bench_nws$nreps_SSN[2],
  bench_nws$preproc_only[2])

bench3 = combine_runtimes_bothmodels(
  bench_res_dir,
  bench_nws$Network[3],
  bench_nws$nreps_S3N[3],
  bench_nws$nreps_SSN[3],
  bench_nws$preproc_only[3])

runtimes_S3N = combine_runtimes_onemodel(bench_res_dir, bench_nws$Network[4], bench_nws$nreps_S3N[4],
                                         "S3N", TRUE)
runtimes_SSN = combine_runtimes_onemodel(bench_res_dir, bench_nws$Network[4], bench_nws$nreps_SSN[4],
                                         "SSN", TRUE)

network = bench_nws$Network[4]
nreps = bench_nws$nreps_S3N[4]
model = "S3N"
preproc_only = TRUE

bench4 = combine_runtimes_bothmodels(
  bench_res_dir,
  bench_nws$Network[4],
  bench_nws$nreps_S3N[4],
  bench_nws$nreps_SSN[4],
  bench_nws$preproc_only[4])

bench5 = combine_runtimes_bothmodels(
  bench_res_dir,
  bench_nws$Network[5],
  bench_nws$nreps_S3N[5],
  bench_nws$nreps_SSN[5],
  bench_nws$preproc_only[5])

bench6 = combine_runtimes_bothmodels(
  bench_res_dir,
  bench_nws$Network[6],
  bench_nws$nreps_S3N[6],
  bench_nws$nreps_SSN[6],
  bench_nws$preproc_only[6])

 
nw1_res = get_summary_for_table(1, bench1)
nw2_res = get_summary_for_table(2, bench2)
nw3_res = get_summary_for_table(3, bench3)
nw4_res = get_summary_for_table(4, bench4)
nw5_res = get_summary_for_table(5, bench5)
nw6_res = get_summary_for_table(6, bench6)
nwres = rbind(nw1_res, nw2_res, nw3_res, nw4_res, nw5_res, nw6_res)
nwres$Task = c("Build_LSN", "Stream_updist", "Obs_updist")
nwres_long = nwres
nwres = pivot_wider(nwres, names_from = "Task", values_from = S3N:SSN)
nwres$BuildLSNratio = nwres$SSN_Build_LSN/nwres$S3N_Build_LSN
nwres$Streamratio = nwres$SSN_Stream_updist/nwres$S3N_Stream_updist
nwres$Obsratio = nwres$SSN_Obs_updist/nwres$S3N_Obs_updist
nwres = nwres[,c(1, 2,5,8, 3,6,9, 4,7,10)]
# #   Network S3N_Build_LSN SSN_Build_LSN BuildLSNratio S3N_Stream_updist SSN_Stream_updist Streamratio S3N_Obs_updist SSN_Obs_updist Obsratio
# #     <dbl>         <dbl>         <dbl>         <dbl>             <dbl>             <dbl>       <dbl>          <dbl>          <dbl>    <dbl>
# # 1       1         0.227          1.51          6.62              1.81              1.15       0.635           3.65           6.74     1.85
# # 2       2         0.729          2.99          4.11              2.33              4.73       2.03            3.83          25.2      6.59
# # 3       3         2.79          26.9           9.63              6.68             66.1        9.89            4.08         142.      34.9 
# # 4       4         5.94         157.           26.5              13.0             193.        14.9             4.24         239.      56.5 
# # 5       5        12.5         2412.          192.               28.1            1582.        56.2             6.35         430.      67.7 
# # 6       6        61.1           NA            NA                70.7              NA         NA              18.0           NA       NA   

# # results from the paper as data.frames:
# nwres = data.frame(
#   Network = 1:6,
#   S3N_Build_LSN = c(0.227, 0.729, 2.79, 5.94, 12.5, 61.1),
#   SSN_Build_LSN = c(1.51, 2.99, 26.9, 157, 2412, NA),
#   BuildLSNratio = c(6.62, 4.11, 9.63, 26.5, 192, NA),
#   S3N_Stream_updist = c(1.81, 2.33, 6.68, 13, 28.1, 70.7),
#   SSN_Stream_updist = c(1.15, 4.73, 66.1, 193, 1582, NA),
#   Streamratio = c(0.635, 2.03, 9.89, 14.9, 56.2, NA),
#   S3N_Obs_updist = c(3.65, 3.83, 4.08, 4.24, 6.35, 18.0),
#   SSN_Obs_updist = c(6.74, 25.2, 142, 239, 430, NA),
#   Obsratio = c(1.85, 6.59, 34.9, 56.5, 67.7, NA)
# )
# 
# nwres_long = data.frame(
#   Network = rep(1:6, each = 3),
#   Task = rep(c("Build_LSN", "Stream_updist", "Obs_updist"), 3),
#   S3N = c(0.227, 1.81, 3.65,
#           0.729, 2.33, 3.83,
#           2.79 , 6.68, 4.08,
#           5.94, 13   , 4.24,
#           12.5, 28.1 , 6.35,
#           61.1, 70.7, 18.0 ),
#   SSN = c(1.51, 1.15, 6.74, 
#           2.99, 4.73, 25.2, 
#           26.9, 66.1, 142, 
#           157, 193, 239, 
#           2412, 1582, 430,
#           NA, NA, NA)
# )

bench_res_plot = nwres_long %>%
  left_join(bench_nws, by = "Network") %>%
  mutate(Task = factor(
    ifelse(
      Task == "Build_LSN", 
      "Build stream network",
      ifelse(
        Task == "Stream_updist",
        "Compute stream updist",
        "Compute site updist"
      )), levels = c("Build stream network", 
                     "Compute stream updist", 
                     "Compute site updist")))

bench_long = bench_res_plot %>%
  pivot_longer(cols = S3N:SSN, names_to = "Software", values_to = "Time")

bench_estimation = data.frame(
  Network = rep(1:3, 2),
  Task = rep("Estimation", 3),
  Software = c(rep("S3N", 3), rep("SSN", 3)),
  Time = c(0.02619061, 0.1761315, 0.8290041, 1.6473641, 8.47340318, 912.4498494),
  nobs = bench_nws$nobs[1:3]
)

# make benchmarking plots

# plot_colors = c("#FDC718FF", "#023743FF") # S3N, SSN
# library(fishualize)
# fishualize() # pick the 2nd and 4th colors
# library(fishualize)
plot_colors = fish(12, option = "Centropyge_loricula")[c(5,10)] # S3N, SSN

# ggplot(filter(bench_long, Task != "Compute site updist"), 
bynreach = ggplot(bench_long, 
                  aes(x = log(nreach, base = 10), 
                      y = log(Time, base = 10), color = Software, shape = Software)) +
  facet_wrap(Task ~ .) +
  geom_line() + geom_point() +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  labs(x = TeX("log$_{10}$(Number of reaches)"),
       y = TeX("log$_{10}$(Time in seconds)")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.59, 0.21),
        text = element_text(size = 11))

# ggsave(filename = paste0(bench_res_dir, "benchmark_plot_nreaches.png"),
#        width = 8, height = 3, units = "in")

bynobs = ggplot(filter(bench_long, Task == "Compute site updist"), 
                aes(x = log(nobs, base = 10), 
                    y = log(Time, base = 10), color = Software, shape = Software)) +
  facet_wrap(Task ~ .) +
  geom_line() + geom_point() +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  labs(x = TeX("log$_{10}$(Number of obs. points)"),
       y = TeX("log$_{10}$(Time in seconds)")) +
  theme_bw() +
  theme(legend.position = "none",
        # legend.position.inside = c(0.59, 0.21),
        text = element_text(size = 11))

# ggsave(filename = paste0(bench_res_dir, "benchmark_plot_nobs.png"),
#        width = 2.7, height = 3, units = "in")

estplot = ggplot(bench_estimation, 
                 aes(x = log(nobs, base = 10), 
                     y = log(Time, base = 10), color = Software, shape = Software)) +
  facet_wrap(Task ~ .) +
  geom_line() + geom_point() +
  scale_shape_manual(values = c("S3N" = 16, "SSN" = 1)) + # S3N filled circles, SSN open circles
  scale_colour_manual(values = plot_colors) +
  labs(x = TeX("log$_{10}$(Number of obs. points)"),
       y = TeX("log$_{10}$(Time in seconds)")) +
  theme_bw() +
  theme(legend.position = "none",
        # legend.position.inside = c(0.59, 0.21),
        text = element_text(size = 11))

# ggsave(filename = paste0(bench_res_dir, "benchmark_plot_estimation.png"),
#        width = 2.7, height = 3, units = "in")

bynreach /
  (bynobs | estplot)

ggsave(filename = paste0(bench_res_dir, "benchmark_plot_preproc_and_est.png"),
       width = 8, height = 6, units = "in")



## Combine and summarize validation results -------------------------------

# combine_params_bothmodels() also generates the validation plots in the paper

valid1 = combine_params_bothmodels(bench_res_dir, 
                                   bench_nws$Network[1],
                                   bench_nws$nreps_S3N[1],
                                   bench_nws$nreps_SSN[1])
#   |Parameter |   bias_S3N|   bias_SSN|    sd_S3N|    sd_SSN|
#   |:---------|----------:|----------:|---------:|---------:|
#   |beta_1    |  0.0673622|  0.0565686| 1.3474402| 1.3499650|
#   |beta_2    | -0.0012077| -0.0011177| 0.0124137| 0.0123792|
#   |sigma.sq  | -0.2394252| -0.2042920| 0.7769548| 0.8108830|
#   |tau.sq    |  0.0775426|  0.1608641| 0.2443687| 0.3163467|
#   |lambda    |  1.5051939|  1.6518596| 7.7301693| 4.9518021|

valid2 = combine_params_bothmodels(bench_res_dir, 
                                   bench_nws$Network[2],
                                   bench_nws$nreps_S3N[2],
                                   bench_nws$nreps_SSN[2])
#   |Parameter |   bias_S3N|   bias_SSN|    sd_S3N|    sd_SSN|
#   |:---------|----------:|----------:|---------:|---------:|
#   |beta_1    | -0.2351882| -0.2253276| 0.4234030| 0.4251077|
#   |beta_2    |  0.0017473|  0.0016751| 0.0033948| 0.0033762|
#   |sigma.sq  |  0.0089140| -0.0710719| 0.3778211| 0.4634365|
#   |tau.sq    | -0.0253604|  0.0896720| 0.0968090| 0.2328690|
#   |lambda    | -0.1518889|  0.3572424| 0.8020116| 1.6412659|

valid3 = combine_params_bothmodels(bench_res_dir, 
                                   bench_nws$Network[3],
                                   bench_nws$nreps_S3N[3],
                                   bench_nws$nreps_SSN[3])
#   |Parameter |   bias_S3N|   bias_SSN|    sd_S3N|    sd_SSN|
#   |:---------|----------:|----------:|---------:|---------:|
#   |beta_1    |  0.0594826|  0.0651499| 0.2366170| 0.2349273|
#   |beta_2    | -0.0003354| -0.0003733| 0.0016183| 0.0016102|
#   |sigma.sq  | -0.0551622| -0.0727642| 0.1720123| 0.1924396|
#   |tau.sq    |  0.0062762|  0.0797815| 0.0500061| 0.1252996|
#   |lambda    |  0.0169189|  0.2995400| 0.4036738| 0.6629289|


